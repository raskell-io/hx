//! Persistent compilation server for fast incremental rebuilds.
//!
//! This module provides a long-running GHCi process that can reload modules
//! quickly instead of spawning fresh GHC processes for each build.

use hx_core::{Error, Fix, Result};
use std::collections::HashSet;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;
use tracing::{debug, info, warn};

/// Configuration for the compilation server.
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Source directories to watch
    pub src_dirs: Vec<PathBuf>,
    /// Package databases to use
    pub package_dbs: Vec<PathBuf>,
    /// Packages to expose
    pub packages: Vec<String>,
    /// Path to GHCi executable
    pub ghc_path: PathBuf,
    /// Idle timeout before auto-shutdown (0 = never)
    pub idle_timeout_ms: u64,
    /// GHC language extensions to enable
    pub extensions: Vec<String>,
    /// Additional GHCi options
    pub ghci_options: Vec<String>,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            src_dirs: vec![PathBuf::from("src")],
            package_dbs: Vec::new(),
            packages: Vec::new(),
            ghc_path: PathBuf::from("ghci"),
            idle_timeout_ms: 0, // No timeout by default
            extensions: Vec::new(),
            ghci_options: Vec::new(),
        }
    }
}

/// Result of a reload operation.
#[derive(Debug, Clone)]
pub struct ReloadResult {
    /// Whether the reload succeeded
    pub success: bool,
    /// Modules that were recompiled
    pub modules_recompiled: Vec<String>,
    /// Time taken for the reload
    pub duration: Duration,
    /// Error messages
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
}

/// Result of a type-check operation.
#[derive(Debug, Clone)]
pub struct TypeCheckResult {
    /// Whether the type check succeeded
    pub success: bool,
    /// Error messages
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
    /// Time taken
    pub duration: Duration,
}

/// Status of the compilation server.
#[derive(Debug, Clone)]
pub struct ServerStatus {
    /// Whether the server is running
    pub running: bool,
    /// Number of loaded modules
    pub loaded_modules: usize,
    /// Time since last activity
    pub idle_time: Duration,
    /// GHC version
    pub ghc_version: Option<String>,
    /// Loaded module names
    pub module_names: Vec<String>,
}

/// Internal state of the GHCi process.
struct GhciProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    loaded_modules: HashSet<String>,
    last_activity: Instant,
    ghc_version: Option<String>,
}

/// Parsed output from a GHCi reload command.
type ReloadOutput = (bool, Vec<String>, Vec<String>, Vec<String>);

/// Persistent compilation server.
pub struct CompilationServer {
    inner: Arc<Mutex<Option<GhciProcess>>>,
    project_root: PathBuf,
    config: ServerConfig,
}

impl CompilationServer {
    /// Start a new compilation server.
    pub async fn start(project_root: PathBuf, config: ServerConfig) -> Result<Self> {
        let server = Self {
            inner: Arc::new(Mutex::new(None)),
            project_root,
            config,
        };

        // Start the GHCi process
        server.spawn_ghci().await?;

        Ok(server)
    }

    /// Connect to an existing server or start a new one.
    pub async fn connect_or_start(project_root: PathBuf, config: ServerConfig) -> Result<Self> {
        // Check if server socket exists
        let socket_path = server_socket_path(&project_root);

        if socket_path.exists() {
            // Try to connect to existing server
            // For now, we'll just start a new one since IPC is complex
            info!("Starting new compilation server");
        }

        Self::start(project_root, config).await
    }

    /// Spawn the GHCi process.
    async fn spawn_ghci(&self) -> Result<()> {
        let ghci_path = self.config.ghc_path.display().to_string();

        let mut cmd = Command::new(&ghci_path);

        // Set working directory
        cmd.current_dir(&self.project_root);

        // Add source directories
        for src_dir in &self.config.src_dirs {
            cmd.arg("-i");
            cmd.arg(src_dir);
        }

        // Add package databases
        for db in &self.config.package_dbs {
            cmd.arg("-package-db");
            cmd.arg(db);
        }

        // Add packages
        for pkg in &self.config.packages {
            cmd.arg("-package");
            cmd.arg(pkg);
        }

        // Add extensions
        for ext in &self.config.extensions {
            cmd.arg(format!("-X{}", ext));
        }

        // Add custom GHCi options
        for opt in &self.config.ghci_options {
            cmd.arg(opt);
        }

        // Configure for non-interactive use
        cmd.arg("-ignore-dot-ghci");
        cmd.arg("-fno-ghci-history");
        cmd.arg("-v0"); // Minimal output

        // Set up pipes
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        debug!("Starting GHCi: {:?}", cmd);

        let mut child = cmd.spawn().map_err(|e| Error::ToolchainMissing {
            tool: "ghci".to_string(),
            source: Some(Box::new(e)),
            fixes: vec![Fix::with_command(
                "Ensure GHC is installed",
                "ghcup install ghc",
            )],
        })?;

        let stdin = child.stdin.take().ok_or_else(|| Error::BuildFailed {
            errors: vec!["Failed to capture GHCi stdin".to_string()],
            fixes: vec![],
        })?;

        let stdout = child.stdout.take().ok_or_else(|| Error::BuildFailed {
            errors: vec!["Failed to capture GHCi stdout".to_string()],
            fixes: vec![],
        })?;

        let mut process = GhciProcess {
            child,
            stdin,
            stdout: BufReader::new(stdout),
            loaded_modules: HashSet::new(),
            last_activity: Instant::now(),
            ghc_version: None,
        };

        // Wait for GHCi to be ready and get version
        process.ghc_version = Self::read_ghci_prompt(&mut process.stdout)?;

        *self.inner.lock().await = Some(process);

        info!("Compilation server started");
        Ok(())
    }

    /// Read until we see the GHCi prompt.
    fn read_ghci_prompt(stdout: &mut BufReader<ChildStdout>) -> Result<Option<String>> {
        let mut version = None;
        let mut line = String::new();

        // Read lines until we see Prelude> or Main> or similar
        loop {
            line.clear();
            match stdout.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let trimmed = line.trim();

                    // Check for version info
                    if trimmed.starts_with("GHCi, version") {
                        version = Some(trimmed.to_string());
                    }

                    // Check for prompt
                    if trimmed.ends_with(">") {
                        break;
                    }
                }
                Err(e) => {
                    warn!("Error reading GHCi output: {}", e);
                    break;
                }
            }
        }

        Ok(version)
    }

    /// Reload all modules.
    pub async fn reload(&self) -> Result<ReloadResult> {
        let start = Instant::now();
        let mut guard = self.inner.lock().await;

        let process = guard.as_mut().ok_or_else(|| Error::BuildFailed {
            errors: vec!["Server not running".to_string()],
            fixes: vec![Fix::with_command("Start the server", "hx server start")],
        })?;

        process.last_activity = Instant::now();

        // Send reload command
        writeln!(process.stdin, ":reload").map_err(|e| Error::Io {
            message: "Failed to write to GHCi".to_string(),
            path: None,
            source: e,
        })?;

        process.stdin.flush().map_err(|e| Error::Io {
            message: "Failed to flush GHCi stdin".to_string(),
            path: None,
            source: e,
        })?;

        // Read output until prompt
        let (success, modules_recompiled, errors, warnings) =
            Self::read_reload_output(&mut process.stdout)?;

        // Update loaded modules
        for module in &modules_recompiled {
            process.loaded_modules.insert(module.clone());
        }

        Ok(ReloadResult {
            success,
            modules_recompiled,
            duration: start.elapsed(),
            errors,
            warnings,
        })
    }

    /// Read the output from a reload command.
    fn read_reload_output(stdout: &mut BufReader<ChildStdout>) -> Result<ReloadOutput> {
        let mut modules = Vec::new();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let mut success = true;
        let mut line = String::new();

        loop {
            line.clear();
            match stdout.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let trimmed = line.trim();

                    // Check for prompt (end of output)
                    if trimmed.ends_with(">") {
                        break;
                    }

                    // Parse output
                    if trimmed.contains("Compiling") {
                        // Extract module name
                        if let Some(module) = Self::extract_module_name(trimmed) {
                            modules.push(module);
                        }
                    } else if trimmed.contains("error:") || trimmed.contains("Error:") {
                        success = false;
                        errors.push(trimmed.to_string());
                    } else if trimmed.contains("warning:") || trimmed.contains("Warning:") {
                        warnings.push(trimmed.to_string());
                    } else if trimmed.contains("Failed") {
                        success = false;
                    }
                }
                Err(e) => {
                    warn!("Error reading GHCi output: {}", e);
                    break;
                }
            }
        }

        Ok((success, modules, errors, warnings))
    }

    /// Extract module name from a "Compiling" line.
    fn extract_module_name(line: &str) -> Option<String> {
        // Format: "[1 of 5] Compiling Module.Name"
        if let Some(idx) = line.find("Compiling") {
            let rest = &line[idx + 10..].trim();
            let module_name = rest.split_whitespace().next()?;
            return Some(module_name.to_string());
        }
        None
    }

    /// Type-check a specific file.
    pub async fn type_check(&self, file: &Path) -> Result<TypeCheckResult> {
        let start = Instant::now();
        let mut guard = self.inner.lock().await;

        let process = guard.as_mut().ok_or_else(|| Error::BuildFailed {
            errors: vec!["Server not running".to_string()],
            fixes: vec![Fix::with_command("Start the server", "hx server start")],
        })?;

        process.last_activity = Instant::now();

        // Send type-check command
        writeln!(process.stdin, ":type-check {}", file.display()).map_err(|e| Error::Io {
            message: "Failed to write to GHCi".to_string(),
            path: None,
            source: e,
        })?;

        process.stdin.flush().map_err(|e| Error::Io {
            message: "Failed to flush GHCi stdin".to_string(),
            path: None,
            source: e,
        })?;

        // Read output
        let (success, _, errors, warnings) = Self::read_reload_output(&mut process.stdout)?;

        Ok(TypeCheckResult {
            success,
            errors,
            warnings,
            duration: start.elapsed(),
        })
    }

    /// Get the current server status.
    pub async fn status(&self) -> Result<ServerStatus> {
        let guard = self.inner.lock().await;

        match guard.as_ref() {
            Some(process) => Ok(ServerStatus {
                running: true,
                loaded_modules: process.loaded_modules.len(),
                idle_time: process.last_activity.elapsed(),
                ghc_version: process.ghc_version.clone(),
                module_names: process.loaded_modules.iter().cloned().collect(),
            }),
            None => Ok(ServerStatus {
                running: false,
                loaded_modules: 0,
                idle_time: Duration::ZERO,
                ghc_version: None,
                module_names: Vec::new(),
            }),
        }
    }

    /// Shutdown the server.
    pub async fn shutdown(&self) -> Result<()> {
        let mut guard = self.inner.lock().await;

        if let Some(mut process) = guard.take() {
            // Send quit command
            let _ = writeln!(process.stdin, ":quit");
            let _ = process.stdin.flush();

            // Wait for process to exit
            let _ = process.child.wait();

            info!("Compilation server stopped");
        }

        // Remove socket file
        let socket_path = server_socket_path(&self.project_root);
        if socket_path.exists() {
            let _ = std::fs::remove_file(&socket_path);
        }

        Ok(())
    }

    /// Load a module by name.
    pub async fn load_module(&self, module: &str) -> Result<ReloadResult> {
        let start = Instant::now();
        let mut guard = self.inner.lock().await;

        let process = guard.as_mut().ok_or_else(|| Error::BuildFailed {
            errors: vec!["Server not running".to_string()],
            fixes: vec![Fix::with_command("Start the server", "hx server start")],
        })?;

        process.last_activity = Instant::now();

        // Send load command
        writeln!(process.stdin, ":load {}", module).map_err(|e| Error::Io {
            message: "Failed to write to GHCi".to_string(),
            path: None,
            source: e,
        })?;

        process.stdin.flush().map_err(|e| Error::Io {
            message: "Failed to flush GHCi stdin".to_string(),
            path: None,
            source: e,
        })?;

        // Read output
        let (success, modules_recompiled, errors, warnings) =
            Self::read_reload_output(&mut process.stdout)?;

        // Update loaded modules
        for m in &modules_recompiled {
            process.loaded_modules.insert(m.clone());
        }

        Ok(ReloadResult {
            success,
            modules_recompiled,
            duration: start.elapsed(),
            errors,
            warnings,
        })
    }

    /// Check if the server is running.
    pub async fn is_running(&self) -> bool {
        let guard = self.inner.lock().await;
        guard.is_some()
    }
}

impl Drop for CompilationServer {
    fn drop(&mut self) {
        // Best-effort cleanup
        if let Ok(mut guard) = self.inner.try_lock()
            && let Some(mut process) = guard.take()
        {
            let _ = writeln!(process.stdin, ":quit");
            let _ = process.stdin.flush();
            let _ = process.child.wait();
        }
    }
}

/// Get the path to the server socket file for a project.
pub fn server_socket_path(project_root: &Path) -> PathBuf {
    project_root.join(".hx").join("server.sock")
}

/// Check if a compilation server is running for a project.
pub fn is_server_running(project_root: &Path) -> bool {
    let socket_path = server_socket_path(project_root);
    socket_path.exists()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_config_default() {
        let config = ServerConfig::default();
        assert_eq!(config.src_dirs, vec![PathBuf::from("src")]);
        assert!(config.packages.is_empty());
        assert_eq!(config.idle_timeout_ms, 0);
    }

    #[test]
    fn test_server_socket_path() {
        let project = PathBuf::from("/home/user/project");
        let socket = server_socket_path(&project);
        assert_eq!(socket, PathBuf::from("/home/user/project/.hx/server.sock"));
    }

    #[test]
    fn test_extract_module_name() {
        assert_eq!(
            CompilationServer::extract_module_name("[1 of 5] Compiling Data.Text"),
            Some("Data.Text".to_string())
        );
        assert_eq!(
            CompilationServer::extract_module_name("[2 of 5] Compiling Main ( src/Main.hs )"),
            Some("Main".to_string())
        );
        assert_eq!(
            CompilationServer::extract_module_name("Some other output"),
            None
        );
    }

    #[test]
    fn test_server_status_default() {
        let status = ServerStatus {
            running: false,
            loaded_modules: 0,
            idle_time: Duration::ZERO,
            ghc_version: None,
            module_names: Vec::new(),
        };
        assert!(!status.running);
    }
}
