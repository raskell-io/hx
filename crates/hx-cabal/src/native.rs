//! Native GHC build orchestration.
//!
//! This module provides direct GHC invocation for building Haskell projects
//! without going through Cabal. It uses module dependency analysis to compile
//! modules in the correct order with parallel compilation support.

use hx_core::{CommandOutput, CommandRunner, Error, Result};
use hx_solver::{build_module_graph, ModuleGraph, ModuleInfo};
use hx_ui::{Output, Progress, Spinner};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tokio::sync::Semaphore;
use tracing::{debug, info};

// ============================================================================
// Module State Tracking for Incremental Builds
// ============================================================================

/// State of a single module for incremental build tracking.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleState {
    /// SHA256 hash of the source file content
    pub source_hash: String,
    /// Modification time (unix timestamp)
    pub mtime: u64,
    /// Hash of interface files this module depends on
    pub deps_hash: String,
    /// Last successful compile timestamp
    pub compiled_at: u64,
}

/// Build state for incremental builds.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BuildState {
    /// GHC version used
    pub ghc_version: String,
    /// Build flags hash
    pub flags_hash: String,
    /// Per-module state
    pub modules: HashMap<String, ModuleState>,
}

impl BuildState {
    /// Load build state from disk.
    pub fn load(project_root: &Path) -> Option<Self> {
        let path = project_root.join(".hx").join("native-build-state.json");
        let content = std::fs::read_to_string(&path).ok()?;
        serde_json::from_str(&content).ok()
    }

    /// Save build state to disk.
    pub fn save(&self, project_root: &Path) -> Result<()> {
        let hx_dir = project_root.join(".hx");
        std::fs::create_dir_all(&hx_dir).map_err(|e| Error::Io {
            message: "failed to create .hx directory".to_string(),
            path: Some(hx_dir.clone()),
            source: e,
        })?;

        let path = hx_dir.join("native-build-state.json");
        let content = serde_json::to_string_pretty(self).map_err(|e| Error::Config {
            message: format!("failed to serialize build state: {}", e),
            path: None,
            source: None,
            fixes: vec![],
        })?;

        std::fs::write(&path, content).map_err(|e| Error::Io {
            message: "failed to write build state".to_string(),
            path: Some(path),
            source: e,
        })?;

        Ok(())
    }

    /// Check if a module needs recompilation.
    pub fn needs_rebuild(
        &self,
        module_name: &str,
        source_path: &Path,
        deps: &[String],
        ghc_version: &str,
        flags_hash: &str,
    ) -> bool {
        // Rebuild if GHC version changed
        if self.ghc_version != ghc_version {
            debug!("Module {} needs rebuild: GHC version changed", module_name);
            return true;
        }

        // Rebuild if flags changed
        if self.flags_hash != flags_hash {
            debug!("Module {} needs rebuild: flags changed", module_name);
            return true;
        }

        // Get previous module state
        let state = match self.modules.get(module_name) {
            Some(s) => s,
            None => {
                debug!("Module {} needs rebuild: no previous state", module_name);
                return true;
            }
        };

        // Check if source file changed
        match compute_file_hash(source_path) {
            Ok(hash) if hash != state.source_hash => {
                debug!("Module {} needs rebuild: source changed", module_name);
                return true;
            }
            Err(_) => {
                debug!("Module {} needs rebuild: cannot hash source", module_name);
                return true;
            }
            _ => {}
        }

        // Check if any dependency was recompiled more recently
        let deps_hash = compute_deps_hash(deps, &self.modules);
        if deps_hash != state.deps_hash {
            debug!("Module {} needs rebuild: deps changed", module_name);
            return true;
        }

        false
    }

    /// Update state for a successfully compiled module.
    pub fn mark_compiled(
        &mut self,
        module_name: &str,
        source_path: &Path,
        deps: &[String],
    ) {
        let source_hash = compute_file_hash(source_path).unwrap_or_default();
        let mtime = source_path
            .metadata()
            .and_then(|m| m.modified())
            .map(|t| t.duration_since(UNIX_EPOCH).map(|d| d.as_secs()).unwrap_or(0))
            .unwrap_or(0);
        let deps_hash = compute_deps_hash(deps, &self.modules);
        let compiled_at = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        self.modules.insert(
            module_name.to_string(),
            ModuleState {
                source_hash,
                mtime,
                deps_hash,
                compiled_at,
            },
        );
    }
}

/// Compute SHA256 hash of a file.
fn compute_file_hash(path: &Path) -> std::io::Result<String> {
    let content = std::fs::read(path)?;
    let mut hasher = Sha256::new();
    hasher.update(&content);
    Ok(format!("{:x}", hasher.finalize()))
}

/// Compute hash of module dependencies.
fn compute_deps_hash(deps: &[String], module_states: &HashMap<String, ModuleState>) -> String {
    let mut hasher = Sha256::new();
    for dep in deps {
        if let Some(state) = module_states.get(dep) {
            hasher.update(dep.as_bytes());
            hasher.update(state.compiled_at.to_le_bytes());
        }
    }
    format!("{:x}", hasher.finalize())
}

/// Compute hash of build flags.
pub fn compute_flags_hash(options: &NativeBuildOptions) -> String {
    let mut hasher = Sha256::new();
    hasher.update(options.optimization.to_le_bytes());
    hasher.update(if options.warnings { &[1u8] } else { &[0u8] });
    hasher.update(if options.werror { &[1u8] } else { &[0u8] });
    for flag in &options.extra_flags {
        hasher.update(flag.as_bytes());
    }
    format!("{:x}", hasher.finalize())
}

// ============================================================================
// GHC Configuration
// ============================================================================

/// GHC compiler configuration.
#[derive(Debug, Clone)]
pub struct GhcConfig {
    /// Path to GHC executable (defaults to "ghc" on PATH)
    pub ghc_path: PathBuf,
    /// GHC version string
    pub version: String,
    /// Package databases to use
    pub package_dbs: Vec<PathBuf>,
    /// Packages to expose
    pub packages: Vec<String>,
}

impl Default for GhcConfig {
    fn default() -> Self {
        Self {
            ghc_path: PathBuf::from("ghc"),
            version: String::new(),
            package_dbs: Vec::new(),
            packages: Vec::new(),
        }
    }
}

impl GhcConfig {
    /// Create a new GHC config, detecting version from PATH.
    pub async fn detect() -> Result<Self> {
        let runner = CommandRunner::new();
        let output = runner.run("ghc", ["--numeric-version"]).await?;

        if !output.success() {
            return Err(Error::ToolchainMissing {
                tool: "ghc".to_string(),
                source: None,
                fixes: vec![],
            });
        }

        let version = output.stdout.trim().to_string();

        // Auto-detect package databases
        let package_dbs = detect_package_dbs(&version).await;

        Ok(Self {
            ghc_path: PathBuf::from("ghc"),
            version,
            package_dbs,
            packages: Vec::new(),
        })
    }

    /// Add a package database.
    pub fn with_package_db(mut self, db: PathBuf) -> Self {
        self.package_dbs.push(db);
        self
    }

    /// Add a package to expose.
    pub fn with_package(mut self, pkg: impl Into<String>) -> Self {
        self.packages.push(pkg.into());
        self
    }

    /// Add multiple packages to expose.
    pub fn with_packages(mut self, pkgs: impl IntoIterator<Item = String>) -> Self {
        self.packages.extend(pkgs);
        self
    }
}

/// Detect available GHC package databases.
async fn detect_package_dbs(ghc_version: &str) -> Vec<PathBuf> {
    let mut dbs = Vec::new();

    // Try to get package databases from ghc-pkg
    let runner = CommandRunner::new();
    if let Ok(output) = runner.run("ghc-pkg", ["list", "--global"]).await
        && output.success()
    {
        // First line is usually the database path
        if let Some(first_line) = output.stdout.lines().next() {
            let db_path = first_line.trim().trim_end_matches(':');
            if !db_path.is_empty() {
                let path = PathBuf::from(db_path);
                if path.exists() {
                    debug!("Found global package db: {}", path.display());
                    dbs.push(path);
                }
            }
        }
    }

    // Add cabal store package database
    if let Some(home) = dirs::home_dir() {
        // Standard cabal store location
        let cabal_store = home
            .join(".cabal")
            .join("store")
            .join(format!("ghc-{}", ghc_version))
            .join("package.db");
        if cabal_store.exists() {
            debug!("Found cabal store db: {}", cabal_store.display());
            dbs.push(cabal_store);
        }

        // hx-managed store location
        let hx_store = home
            .join(".cache")
            .join("hx")
            .join("cabal")
            .join("store")
            .join(format!("ghc-{}", ghc_version))
            .join("package.db");
        if hx_store.exists() {
            debug!("Found hx store db: {}", hx_store.display());
            dbs.push(hx_store);
        }
    }

    dbs
}

/// Get packages needed from a lockfile.
pub fn packages_from_lockfile(lockfile_path: &Path) -> Vec<String> {
    let content = match std::fs::read_to_string(lockfile_path) {
        Ok(c) => c,
        Err(_) => return vec!["base".to_string()],
    };

    // Parse the lockfile TOML to extract package names
    let mut packages = vec!["base".to_string()];

    // Simple parsing - look for [[packages]] sections
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("name = ") {
            let name = trimmed
                .trim_start_matches("name = ")
                .trim_matches('"')
                .to_string();
            if !name.is_empty() && !packages.contains(&name) {
                packages.push(name);
            }
        }
    }

    packages
}

/// Options for native GHC builds.
#[derive(Debug, Clone)]
pub struct NativeBuildOptions {
    /// Source directories to search
    pub src_dirs: Vec<PathBuf>,
    /// Output directory for .hi and .o files
    pub output_dir: PathBuf,
    /// GHC optimization level (0, 1, or 2)
    pub optimization: u8,
    /// Enable all warnings
    pub warnings: bool,
    /// Treat warnings as errors
    pub werror: bool,
    /// Additional GHC flags
    pub extra_flags: Vec<String>,
    /// Number of parallel jobs
    pub jobs: usize,
    /// Verbose output
    pub verbose: bool,
    /// Main module name (for executables)
    pub main_module: Option<String>,
    /// Output executable path
    pub output_exe: Option<PathBuf>,
}

impl Default for NativeBuildOptions {
    fn default() -> Self {
        Self {
            src_dirs: vec![PathBuf::from("src")],
            output_dir: PathBuf::from(".hx/build"),
            optimization: 1,
            warnings: true,
            werror: false,
            extra_flags: Vec::new(),
            jobs: num_cpus::get(),
            verbose: false,
            main_module: None,
            output_exe: None,
        }
    }
}

/// Result of compiling a single module.
#[derive(Debug, Clone)]
pub struct ModuleCompileResult {
    /// Module name
    pub module: String,
    /// Whether compilation succeeded
    pub success: bool,
    /// Compilation duration
    pub duration: Duration,
    /// Path to object file (if successful)
    pub object_file: Option<PathBuf>,
    /// Path to interface file (if successful)
    pub interface_file: Option<PathBuf>,
    /// Warnings from compilation
    pub warnings: Vec<String>,
    /// Errors (if failed)
    pub errors: Vec<String>,
}

/// Result of a native build.
#[derive(Debug)]
pub struct NativeBuildResult {
    /// Whether the build succeeded
    pub success: bool,
    /// Total build duration
    pub duration: Duration,
    /// Modules compiled
    pub modules_compiled: usize,
    /// Modules skipped (already up to date)
    pub modules_skipped: usize,
    /// Individual module results
    pub module_results: Vec<ModuleCompileResult>,
    /// Output executable path (if linking)
    pub executable: Option<PathBuf>,
    /// Errors
    pub errors: Vec<String>,
    /// Warnings
    pub warnings: Vec<String>,
}

/// Native GHC build orchestrator.
pub struct NativeBuilder {
    ghc: GhcConfig,
}

impl NativeBuilder {
    /// Create a new native builder.
    pub fn new(ghc: GhcConfig) -> Self {
        Self { ghc }
    }

    /// Create a native builder with auto-detected GHC.
    pub async fn detect() -> Result<Self> {
        let ghc = GhcConfig::detect().await?;
        Ok(Self::new(ghc))
    }

    /// Build a Haskell project.
    pub async fn build(
        &self,
        project_root: &Path,
        options: &NativeBuildOptions,
        output: &Output,
    ) -> Result<NativeBuildResult> {
        let start = Instant::now();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let mut module_results = Vec::new();

        // Build module dependency graph
        let src_dirs: Vec<PathBuf> = options
            .src_dirs
            .iter()
            .map(|d| project_root.join(d))
            .collect();

        info!("Building module graph from {:?}", src_dirs);
        let graph = build_module_graph(&src_dirs).map_err(|e| Error::BuildFailed {
            errors: vec![format!("Failed to build module graph: {}", e)],
            fixes: vec![],
        })?;

        let module_count = graph.modules.len();
        if module_count == 0 {
            output.info("No Haskell modules found");
            return Ok(NativeBuildResult {
                success: true,
                duration: start.elapsed(),
                modules_compiled: 0,
                modules_skipped: 0,
                module_results: vec![],
                executable: None,
                errors: vec![],
                warnings: vec![],
            });
        }

        info!("Found {} modules", module_count);

        // Get parallel compilation groups
        let groups = graph.parallel_groups().map_err(|e| Error::BuildFailed {
            errors: vec![format!("Cycle detected in modules: {}", e)],
            fixes: vec![],
        })?;

        // Ensure output directory exists
        let output_dir = project_root.join(&options.output_dir);
        std::fs::create_dir_all(&output_dir).map_err(|e| Error::Io {
            message: "failed to create build directory".to_string(),
            path: Some(output_dir.clone()),
            source: e,
        })?;

        // Load previous build state for incremental builds
        let mut build_state = BuildState::load(project_root).unwrap_or_default();
        let flags_hash = compute_flags_hash(options);

        // Determine which modules need recompilation
        let modules_needing_rebuild: HashSet<String> = graph
            .modules
            .iter()
            .filter(|(name, info)| {
                let deps = graph.dependencies.get(*name).map(|d| d.as_slice()).unwrap_or(&[]);
                build_state.needs_rebuild(
                    name,
                    &info.path,
                    deps,
                    &self.ghc.version,
                    &flags_hash,
                )
            })
            .map(|(name, _)| name.clone())
            .collect();

        // If a module needs rebuild, all its dependents also need rebuild
        let mut all_rebuild: HashSet<String> = modules_needing_rebuild.clone();
        for module_name in &modules_needing_rebuild {
            collect_dependents(&graph, module_name, &mut all_rebuild);
        }

        let rebuild_count = all_rebuild.len();
        let skip_count = module_count - rebuild_count;

        if rebuild_count == 0 {
            output.info(&format!("All {} modules up to date", module_count));
            return Ok(NativeBuildResult {
                success: true,
                duration: start.elapsed(),
                modules_compiled: 0,
                modules_skipped: module_count,
                module_results: vec![],
                executable: options.output_exe.as_ref().map(|p| project_root.join(p)),
                errors: vec![],
                warnings: vec![],
            });
        }

        info!("{} modules need recompilation, {} up to date", rebuild_count, skip_count);

        // Track compiled modules
        let mut compiled_modules = HashSet::new();
        let mut modules_compiled = 0;

        // Use progress bar for compilation (determinate progress)
        let progress = if !options.verbose && rebuild_count > 0 {
            Some(Progress::new(rebuild_count as u64, "Compiling"))
        } else {
            None
        };

        for (group_idx, group) in groups.iter().enumerate() {
            // Filter to only modules that need rebuild
            let group_to_compile: Vec<String> = group
                .iter()
                .filter(|m| all_rebuild.contains(*m))
                .cloned()
                .collect();

            if group_to_compile.is_empty() {
                continue;
            }

            debug!(
                "Compiling group {} with {} modules",
                group_idx,
                group_to_compile.len()
            );

            // Compile all modules in this group in parallel
            let group_results = self
                .compile_group(&graph, &group_to_compile, project_root, options, &compiled_modules)
                .await?;

            for result in group_results {
                if result.success {
                    // Update build state for successfully compiled module
                    let deps = graph
                        .dependencies
                        .get(&result.module)
                        .map(|d| d.as_slice())
                        .unwrap_or(&[]);
                    if let Some(info) = graph.modules.get(&result.module) {
                        build_state.mark_compiled(&result.module, &info.path, deps);
                    }
                    compiled_modules.insert(result.module.clone());
                    modules_compiled += 1;

                    // Update progress bar
                    if let Some(ref p) = progress {
                        p.inc(1);
                    }
                } else {
                    errors.extend(result.errors.clone());
                }
                warnings.extend(result.warnings.clone());
                module_results.push(result);
            }

            // If any module failed, stop
            if !errors.is_empty() {
                break;
            }
        }

        // Finish progress bar
        if let Some(p) = progress {
            if errors.is_empty() {
                p.finish(format!(
                    "Compiled {} modules in {} ({} skipped)",
                    modules_compiled,
                    format_duration(start.elapsed()),
                    skip_count
                ));
            } else {
                // Progress bar will be dropped, but we show error separately
                drop(p);
            }
        }

        // Save build state if successful
        if errors.is_empty() {
            build_state.ghc_version = self.ghc.version.clone();
            build_state.flags_hash = flags_hash;
            if let Err(e) = build_state.save(project_root) {
                debug!("Failed to save build state: {}", e);
            }
        }

        // Link if requested and compilation succeeded
        let executable = if errors.is_empty()
            && options.main_module.is_some()
            && options.output_exe.is_some()
        {
            self.link(project_root, &graph, options, &output_dir, output)
                .await?
        } else {
            None
        };

        Ok(NativeBuildResult {
            success: errors.is_empty(),
            duration: start.elapsed(),
            modules_compiled,
            modules_skipped: 0, // TODO: incremental
            module_results,
            executable,
            errors,
            warnings,
        })
    }

    /// Compile a group of independent modules in parallel.
    async fn compile_group(
        &self,
        graph: &ModuleGraph,
        modules: &[String],
        project_root: &Path,
        options: &NativeBuildOptions,
        _compiled: &HashSet<String>,
    ) -> Result<Vec<ModuleCompileResult>> {
        let semaphore = Arc::new(Semaphore::new(options.jobs));
        let mut handles = Vec::new();

        for module_name in modules {
            let module_info = match graph.modules.get(module_name) {
                Some(info) => info.clone(),
                None => continue,
            };

            let sem = Arc::clone(&semaphore);
            let ghc = self.ghc.clone();
            let project_root = project_root.to_path_buf();
            let options = options.clone();

            handles.push(tokio::spawn(async move {
                let _permit = sem.acquire().await.unwrap();
                compile_module(&ghc, &module_info, &project_root, &options).await
            }));
        }

        let mut results = Vec::new();
        for handle in handles {
            match handle.await {
                Ok(result) => results.push(result),
                Err(e) => {
                    results.push(ModuleCompileResult {
                        module: "unknown".to_string(),
                        success: false,
                        duration: Duration::ZERO,
                        object_file: None,
                        interface_file: None,
                        warnings: vec![],
                        errors: vec![format!("Task failed: {}", e)],
                    });
                }
            }
        }

        Ok(results)
    }

    /// Link compiled modules into an executable.
    async fn link(
        &self,
        project_root: &Path,
        graph: &ModuleGraph,
        options: &NativeBuildOptions,
        output_dir: &Path,
        output: &Output,
    ) -> Result<Option<PathBuf>> {
        let exe_path = match &options.output_exe {
            Some(p) => project_root.join(p),
            None => return Ok(None),
        };

        let main_module = match &options.main_module {
            Some(m) => m,
            None => return Ok(None),
        };

        info!("Linking executable: {:?}", exe_path);

        let spinner = if !options.verbose {
            Some(Spinner::new("Linking..."))
        } else {
            None
        };

        // Collect all object files
        let mut object_files = Vec::new();
        for module_name in graph.modules.keys() {
            let obj_file = output_dir.join(format!("{}.o", module_name.replace('.', "/")));
            if obj_file.exists() {
                object_files.push(obj_file);
            }
        }

        // Build GHC link command
        let mut args: Vec<String> = vec!["-o".to_string(), exe_path.display().to_string()];

        // Add package databases
        for db in &self.ghc.package_dbs {
            args.push(format!("-package-db={}", db.display()));
        }

        // Add packages
        for pkg in &self.ghc.packages {
            args.push("-package".to_string());
            args.push(pkg.clone());
        }

        // Add optimization
        if options.optimization > 0 {
            args.push(format!("-O{}", options.optimization));
        }

        // Add object files
        for obj in &object_files {
            args.push(obj.display().to_string());
        }

        // Add main module hint
        args.push("-main-is".to_string());
        args.push(main_module.clone());

        let runner = CommandRunner::new().with_working_dir(project_root);
        let ghc_path = self.ghc.ghc_path.display().to_string();
        let cmd_output = runner
            .run(&ghc_path, &args)
            .await?;

        if let Some(spinner) = spinner {
            if cmd_output.success() {
                spinner.finish_success("Linked");
            } else {
                spinner.finish_error("Link failed");
            }
        }

        if !cmd_output.success() {
            output.info(&cmd_output.stderr);
            return Err(Error::BuildFailed {
                errors: vec!["Linking failed".to_string()],
                fixes: vec![],
            });
        }

        Ok(Some(exe_path))
    }
}

/// Compile a single Haskell module.
async fn compile_module(
    ghc: &GhcConfig,
    module: &ModuleInfo,
    project_root: &Path,
    options: &NativeBuildOptions,
) -> ModuleCompileResult {
    let start = Instant::now();
    let module_name = &module.name;

    debug!("Compiling module: {}", module_name);

    // Build output paths
    let output_dir = project_root.join(&options.output_dir);
    let module_subdir = module_name.replace('.', "/");
    let hi_dir = output_dir.join("hi");
    let o_dir = output_dir.join("o");

    // Ensure directories exist
    let module_hi_dir = hi_dir.join(std::path::Path::new(&module_subdir).parent().unwrap_or(std::path::Path::new("")));
    let module_o_dir = o_dir.join(std::path::Path::new(&module_subdir).parent().unwrap_or(std::path::Path::new("")));

    if let Err(e) = std::fs::create_dir_all(&module_hi_dir) {
        return ModuleCompileResult {
            module: module_name.clone(),
            success: false,
            duration: start.elapsed(),
            object_file: None,
            interface_file: None,
            warnings: vec![],
            errors: vec![format!("Failed to create directory: {}", e)],
        };
    }
    if let Err(e) = std::fs::create_dir_all(&module_o_dir) {
        return ModuleCompileResult {
            module: module_name.clone(),
            success: false,
            duration: start.elapsed(),
            object_file: None,
            interface_file: None,
            warnings: vec![],
            errors: vec![format!("Failed to create directory: {}", e)],
        };
    }

    // Build GHC args
    let mut args: Vec<String> = vec![
        "-c".to_string(), // Compile only
        format!("-hidir={}", hi_dir.display()),
        format!("-odir={}", o_dir.display()),
    ];

    // Add source directories
    for src_dir in &options.src_dirs {
        args.push(format!("-i{}", project_root.join(src_dir).display()));
    }

    // Add package databases
    for db in &ghc.package_dbs {
        args.push(format!("-package-db={}", db.display()));
    }

    // Add packages
    for pkg in &ghc.packages {
        args.push("-package".to_string());
        args.push(pkg.clone());
    }

    // Add optimization
    if options.optimization > 0 {
        args.push(format!("-O{}", options.optimization));
    }

    // Add warnings
    if options.warnings {
        args.push("-Wall".to_string());
    }
    if options.werror {
        args.push("-Werror".to_string());
    }

    // Add extra flags
    args.extend(options.extra_flags.clone());

    // Add source file
    args.push(module.path.display().to_string());

    // Run GHC
    let runner = CommandRunner::new().with_working_dir(project_root);
    let ghc_path = ghc.ghc_path.display().to_string();
    let output = match runner.run(&ghc_path, &args).await {
        Ok(o) => o,
        Err(e) => {
            return ModuleCompileResult {
                module: module_name.clone(),
                success: false,
                duration: start.elapsed(),
                object_file: None,
                interface_file: None,
                warnings: vec![],
                errors: vec![format!("Failed to run GHC: {}", e)],
            };
        }
    };

    // Parse output
    let (warnings, errors) = parse_ghc_output(&output);

    let success = output.success();
    let object_file = if success {
        Some(o_dir.join(format!("{}.o", module_subdir)))
    } else {
        None
    };
    let interface_file = if success {
        Some(hi_dir.join(format!("{}.hi", module_subdir)))
    } else {
        None
    };

    ModuleCompileResult {
        module: module_name.clone(),
        success,
        duration: start.elapsed(),
        object_file,
        interface_file,
        warnings,
        errors,
    }
}

/// Parse GHC output into warnings and errors.
fn parse_ghc_output(output: &CommandOutput) -> (Vec<String>, Vec<String>) {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    let combined = format!("{}\n{}", output.stdout, output.stderr);

    for line in combined.lines() {
        let trimmed = line.trim();
        if trimmed.contains(": error:") || trimmed.contains(": Error:") {
            errors.push(line.to_string());
        } else if trimmed.contains(": warning:") || trimmed.contains(": Warning:") {
            warnings.push(line.to_string());
        } else if !errors.is_empty() && (trimmed.starts_with(' ') || trimmed.starts_with('\t')) {
            // Continuation of previous error
            if let Some(last) = errors.last_mut() {
                last.push('\n');
                last.push_str(line);
            }
        }
    }

    (warnings, errors)
}

/// Collect all modules that depend on a given module (transitively).
fn collect_dependents(graph: &ModuleGraph, module: &str, dependents: &mut HashSet<String>) {
    for (name, deps) in &graph.dependencies {
        if deps.contains(&module.to_string()) && !dependents.contains(name) {
            dependents.insert(name.clone());
            // Recursively collect dependents of this dependent
            collect_dependents(graph, name, dependents);
        }
    }
}

fn format_duration(duration: Duration) -> String {
    let secs = duration.as_secs_f64();
    if secs < 1.0 {
        format!("{:.0}ms", duration.as_millis())
    } else if secs < 60.0 {
        format!("{:.1}s", secs)
    } else {
        format!("{:.1}m", secs / 60.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ghc_config_default() {
        let config = GhcConfig::default();
        assert_eq!(config.ghc_path, PathBuf::from("ghc"));
        assert!(config.packages.is_empty());
    }

    #[test]
    fn test_native_build_options_default() {
        let options = NativeBuildOptions::default();
        assert_eq!(options.optimization, 1);
        assert!(options.warnings);
        assert!(!options.werror);
    }

    #[test]
    fn test_parse_ghc_output_error() {
        let output = CommandOutput {
            exit_code: 1,
            stdout: String::new(),
            stderr: "src/Main.hs:10:5: error:\n    Variable not in scope: foo".to_string(),
            duration: Duration::from_secs(1),
        };
        let (warnings, errors) = parse_ghc_output(&output);
        assert!(warnings.is_empty());
        assert_eq!(errors.len(), 1);
        assert!(errors[0].contains("error:"));
    }

    #[test]
    fn test_parse_ghc_output_warning() {
        let output = CommandOutput {
            exit_code: 0,
            stdout: String::new(),
            stderr: "src/Lib.hs:5:1: warning: [-Wunused-imports]\n    The import of 'Data.List' is redundant".to_string(),
            duration: Duration::from_secs(1),
        };
        let (warnings, errors) = parse_ghc_output(&output);
        assert_eq!(warnings.len(), 1);
        assert!(errors.is_empty());
        assert!(warnings[0].contains("warning:"));
    }
}
