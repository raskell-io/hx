//! Command execution utilities.

use std::ffi::OsStr;
use std::path::Path;
use std::process::Stdio;
use std::time::{Duration, Instant};
use tokio::process::Command;
use tracing::{debug, instrument};

use crate::error::{Error, Fix};

/// Output from a command execution.
#[derive(Debug, Clone)]
pub struct CommandOutput {
    /// Exit code (0 = success)
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// How long the command took
    pub duration: Duration,
}

impl CommandOutput {
    /// Check if the command succeeded.
    pub fn success(&self) -> bool {
        self.exit_code == 0
    }
}

/// A command runner that captures output and provides structured results.
#[derive(Debug, Clone, Default)]
pub struct CommandRunner {
    /// Working directory for commands
    pub working_dir: Option<std::path::PathBuf>,
    /// Environment variables to set
    pub env: Vec<(String, String)>,
    /// Whether to inherit the parent environment
    pub inherit_env: bool,
}

impl CommandRunner {
    /// Create a new command runner.
    pub fn new() -> Self {
        Self {
            working_dir: None,
            env: Vec::new(),
            inherit_env: true,
        }
    }

    /// Set the working directory.
    pub fn with_working_dir(mut self, dir: impl AsRef<Path>) -> Self {
        self.working_dir = Some(dir.as_ref().to_path_buf());
        self
    }

    /// Add an environment variable.
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.push((key.into(), value.into()));
        self
    }

    /// Configure PATH to use a specific toolchain bin directory.
    ///
    /// Prepends the given bin directory to PATH, ensuring tools from that
    /// directory are found first. Can be called multiple times to add
    /// multiple bin directories.
    pub fn with_ghc_bin(mut self, bin_dir: impl AsRef<Path>) -> Self {
        // Check if we already have a PATH in our env list
        let current_path = self
            .env
            .iter()
            .rev()
            .find(|(k, _)| k == "PATH")
            .map(|(_, v)| v.clone())
            .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());

        let bin_str = bin_dir.as_ref().to_string_lossy();

        #[cfg(windows)]
        let separator = ";";
        #[cfg(not(windows))]
        let separator = ":";

        let new_path = format!("{}{}{}", bin_str, separator, current_path);
        self.env.push(("PATH".into(), new_path));
        self
    }

    /// Run a command and capture output.
    #[instrument(skip(self, args), fields(program = %program.as_ref().to_string_lossy()))]
    pub async fn run<S, I>(&self, program: S, args: I) -> Result<CommandOutput, Error>
    where
        S: AsRef<OsStr>,
        I: IntoIterator<Item = S>,
    {
        let program_ref = program.as_ref();
        let args_vec: Vec<_> = args
            .into_iter()
            .map(|a| a.as_ref().to_os_string())
            .collect();

        debug!(
            "Running command: {} {:?}",
            program_ref.to_string_lossy(),
            args_vec
        );

        let mut cmd = Command::new(program_ref);
        cmd.args(&args_vec)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        if let Some(ref dir) = self.working_dir {
            cmd.current_dir(dir);
        }

        if !self.inherit_env {
            cmd.env_clear();
        }

        for (key, value) in &self.env {
            cmd.env(key, value);
        }

        let start = Instant::now();

        let output = cmd.output().await.map_err(|e| {
            let program_str = program_ref.to_string_lossy().to_string();
            if e.kind() == std::io::ErrorKind::NotFound {
                Error::ToolchainMissing {
                    tool: program_str.clone(),
                    source: Some(Box::new(e)),
                    fixes: vec![Fix::with_command(
                        format!("Install {}", program_str),
                        "hx toolchain install".to_string(),
                    )],
                }
            } else {
                Error::Io {
                    message: format!("failed to execute {}", program_str),
                    path: None,
                    source: e,
                }
            }
        })?;

        let duration = start.elapsed();

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let exit_code = output.status.code().unwrap_or(-1);

        debug!(
            exit_code = exit_code,
            duration_ms = duration.as_millis(),
            "Command completed"
        );

        Ok(CommandOutput {
            exit_code,
            stdout,
            stderr,
            duration,
        })
    }

    /// Run a command and return an error if it fails.
    pub async fn run_checked<S, I>(&self, program: S, args: I) -> Result<CommandOutput, Error>
    where
        S: AsRef<OsStr>,
        I: IntoIterator<Item = S>,
    {
        let program_str = program.as_ref().to_string_lossy().to_string();
        let output = self.run(program, args).await?;

        if !output.success() {
            return Err(Error::CommandFailed {
                command: program_str,
                exit_code: Some(output.exit_code),
                stdout: output.stdout,
                stderr: output.stderr,
                fixes: vec![],
            });
        }

        Ok(output)
    }
}
