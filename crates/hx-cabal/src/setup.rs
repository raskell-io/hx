//! Custom Setup.hs support for packages with `build-type: Custom`.
//!
//! This module handles compiling and running Setup.hs files for packages
//! that require custom build logic.

use hx_core::{CommandRunner, Error, Fix, Result};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tracing::{debug, info};

/// Configuration for building a custom Setup.hs.
#[derive(Debug, Clone, Default)]
pub struct CustomSetupOptions {
    /// Path to GHC executable
    pub ghc_path: PathBuf,
    /// Package databases to use
    pub package_dbs: Vec<PathBuf>,
    /// Extra packages to expose (from setup-depends)
    pub setup_depends: Vec<String>,
    /// Build directory for intermediate files
    pub build_dir: PathBuf,
    /// Enable verbose output
    pub verbose: bool,
}

/// Result of compiling Setup.hs.
#[derive(Debug, Clone)]
pub struct SetupCompileResult {
    /// Path to the compiled Setup executable
    pub executable_path: PathBuf,
    /// Whether compilation succeeded
    pub success: bool,
    /// Compilation duration
    pub duration: Duration,
    /// Error messages if failed
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
}

/// Result of running a Setup command.
#[derive(Debug, Clone)]
pub struct SetupRunResult {
    /// Whether the command succeeded
    pub success: bool,
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Command duration
    pub duration: Duration,
}

/// Configuration for Setup.hs configure command.
#[derive(Debug, Clone, Default)]
pub struct ConfigureFlags {
    /// Installation prefix
    pub prefix: Option<PathBuf>,
    /// Library directory
    pub libdir: Option<PathBuf>,
    /// Binary directory
    pub bindir: Option<PathBuf>,
    /// Data directory
    pub datadir: Option<PathBuf>,
    /// Package database to register to
    pub package_db: Option<PathBuf>,
    /// GHC path
    pub with_ghc: Option<PathBuf>,
    /// Extra configure flags
    pub extra_flags: Vec<String>,
}

impl ConfigureFlags {
    /// Convert flags to command-line arguments.
    pub fn to_args(&self) -> Vec<String> {
        let mut args = Vec::new();

        if let Some(ref prefix) = self.prefix {
            args.push(format!("--prefix={}", prefix.display()));
        }
        if let Some(ref libdir) = self.libdir {
            args.push(format!("--libdir={}", libdir.display()));
        }
        if let Some(ref bindir) = self.bindir {
            args.push(format!("--bindir={}", bindir.display()));
        }
        if let Some(ref datadir) = self.datadir {
            args.push(format!("--datadir={}", datadir.display()));
        }
        if let Some(ref db) = self.package_db {
            args.push(format!("--package-db={}", db.display()));
        }
        if let Some(ref ghc) = self.with_ghc {
            args.push(format!("--with-ghc={}", ghc.display()));
        }

        args.extend(self.extra_flags.clone());
        args
    }
}

/// Configuration for Setup.hs build command.
#[derive(Debug, Clone, Default)]
pub struct BuildFlags {
    /// Number of parallel jobs
    pub jobs: Option<usize>,
    /// Extra build flags
    pub extra_flags: Vec<String>,
}

impl BuildFlags {
    /// Convert flags to command-line arguments.
    pub fn to_args(&self) -> Vec<String> {
        let mut args = Vec::new();

        if let Some(jobs) = self.jobs {
            args.push(format!("-j{}", jobs));
        }

        args.extend(self.extra_flags.clone());
        args
    }
}

/// Configuration for Setup.hs copy command.
#[derive(Debug, Clone, Default)]
pub struct CopyFlags {
    /// Destination directory
    pub destdir: Option<PathBuf>,
    /// Extra copy flags
    pub extra_flags: Vec<String>,
}

impl CopyFlags {
    /// Convert flags to command-line arguments.
    pub fn to_args(&self) -> Vec<String> {
        let mut args = Vec::new();

        if let Some(ref destdir) = self.destdir {
            args.push(format!("--destdir={}", destdir.display()));
        }

        args.extend(self.extra_flags.clone());
        args
    }
}

/// Find the Setup.hs file in a package directory.
pub fn find_setup_file(package_dir: &Path) -> Option<PathBuf> {
    // Try Setup.hs first
    let setup_hs = package_dir.join("Setup.hs");
    if setup_hs.exists() {
        return Some(setup_hs);
    }

    // Then try Setup.lhs
    let setup_lhs = package_dir.join("Setup.lhs");
    if setup_lhs.exists() {
        return Some(setup_lhs);
    }

    None
}

/// Compile Setup.hs to an executable.
pub async fn compile_setup(
    package_dir: &Path,
    options: &CustomSetupOptions,
) -> Result<SetupCompileResult> {
    let start = Instant::now();

    // Find Setup.hs file
    let setup_file = find_setup_file(package_dir).ok_or_else(|| Error::BuildFailed {
        errors: vec![format!(
            "No Setup.hs or Setup.lhs found in package directory: {}",
            package_dir.display()
        )],
        fixes: vec![
            Fix::new("Create a Setup.hs file with:"),
            Fix::new("  import Distribution.Simple"),
            Fix::new("  main = defaultMain"),
            Fix::new("Or use 'build-type: Simple' in your .cabal file to avoid custom setup"),
        ],
    })?;

    info!("Compiling {}", setup_file.display());

    // Ensure build directory exists
    std::fs::create_dir_all(&options.build_dir).map_err(|e| Error::Io {
        message: "failed to create setup build directory".to_string(),
        path: Some(options.build_dir.clone()),
        source: e,
    })?;

    // Output path for compiled Setup executable
    let setup_exe = options
        .build_dir
        .join(if cfg!(windows) { "Setup.exe" } else { "Setup" });

    // Build GHC arguments
    let mut args = vec![
        "--make".to_string(),
        "-o".to_string(),
        setup_exe.display().to_string(),
    ];

    // Add package databases
    for db in &options.package_dbs {
        args.push(format!("-package-db={}", db.display()));
    }

    // Add setup-depends packages
    for pkg in &options.setup_depends {
        args.push("-package".to_string());
        args.push(pkg.clone());
    }

    // Always need Cabal package for Setup.hs
    if !options.setup_depends.iter().any(|p| p.starts_with("Cabal")) {
        args.push("-package".to_string());
        args.push("Cabal".to_string());
    }

    // Add base package
    if !options.setup_depends.iter().any(|p| p.starts_with("base")) {
        args.push("-package".to_string());
        args.push("base".to_string());
    }

    // Add output directory for intermediate files
    args.push(format!("-outputdir={}", options.build_dir.display()));

    // Add the source file
    args.push(setup_file.display().to_string());

    if options.verbose {
        info!(
            "Setup compile: {} {}",
            options.ghc_path.display(),
            args.join(" ")
        );
    }

    let runner = CommandRunner::new().with_working_dir(package_dir);
    let ghc_path = options.ghc_path.display().to_string();
    let output = runner.run(&ghc_path, &args).await?;

    let success = output.success();
    let (warnings, errors) = parse_ghc_output(&output.stdout, &output.stderr);

    if !success {
        debug!("Setup compilation failed: {}", output.stderr);
    }

    Ok(SetupCompileResult {
        executable_path: setup_exe,
        success,
        duration: start.elapsed(),
        errors,
        warnings,
    })
}

/// Run Setup configure.
pub async fn run_configure(
    setup_exe: &Path,
    package_dir: &Path,
    flags: &ConfigureFlags,
    verbose: bool,
) -> Result<SetupRunResult> {
    run_setup_command(
        setup_exe,
        package_dir,
        "configure",
        &flags.to_args(),
        verbose,
    )
    .await
}

/// Run Setup build.
pub async fn run_build(
    setup_exe: &Path,
    package_dir: &Path,
    flags: &BuildFlags,
    verbose: bool,
) -> Result<SetupRunResult> {
    run_setup_command(setup_exe, package_dir, "build", &flags.to_args(), verbose).await
}

/// Run Setup copy.
pub async fn run_copy(
    setup_exe: &Path,
    package_dir: &Path,
    flags: &CopyFlags,
    verbose: bool,
) -> Result<SetupRunResult> {
    run_setup_command(setup_exe, package_dir, "copy", &flags.to_args(), verbose).await
}

/// Run Setup register.
pub async fn run_register(
    setup_exe: &Path,
    package_dir: &Path,
    package_db: Option<&Path>,
    verbose: bool,
) -> Result<SetupRunResult> {
    let mut args = Vec::new();
    if let Some(db) = package_db {
        args.push(format!("--package-db={}", db.display()));
    }
    run_setup_command(setup_exe, package_dir, "register", &args, verbose).await
}

/// Run a Setup.hs command.
async fn run_setup_command(
    setup_exe: &Path,
    working_dir: &Path,
    command: &str,
    args: &[String],
    verbose: bool,
) -> Result<SetupRunResult> {
    let start = Instant::now();

    let mut full_args = vec![command.to_string()];
    full_args.extend(args.iter().cloned());

    if verbose {
        info!("Running: {} {}", setup_exe.display(), full_args.join(" "));
    }

    let runner = CommandRunner::new().with_working_dir(working_dir);
    let output = runner
        .run(&setup_exe.display().to_string(), &full_args)
        .await?;

    let exit_code = output.exit_code;
    let success = output.success();

    if !success {
        debug!(
            "Setup {} failed (exit {}): {}",
            command, exit_code, output.stderr
        );
    }

    Ok(SetupRunResult {
        success,
        exit_code,
        stdout: output.stdout,
        stderr: output.stderr,
        duration: start.elapsed(),
    })
}

/// Build a package using its custom Setup.hs.
///
/// This performs the full custom build workflow:
/// 1. Compile Setup.hs
/// 2. Run ./Setup configure
/// 3. Run ./Setup build
/// 4. Run ./Setup copy (if destdir provided)
/// 5. Run ./Setup register (if package_db provided)
pub async fn build_with_setup(
    package_dir: &Path,
    options: &CustomSetupOptions,
    configure_flags: &ConfigureFlags,
    build_flags: &BuildFlags,
    copy_flags: Option<&CopyFlags>,
    register_db: Option<&Path>,
) -> Result<CustomBuildResult> {
    let start = Instant::now();
    let mut result = CustomBuildResult {
        success: true,
        duration: Duration::ZERO,
        setup_compiled: false,
        configured: false,
        built: false,
        copied: false,
        registered: false,
        errors: Vec::new(),
        warnings: Vec::new(),
    };

    // Step 1: Compile Setup.hs
    let compile_result = compile_setup(package_dir, options).await?;
    result.warnings.extend(compile_result.warnings);

    if !compile_result.success {
        result.success = false;
        result.errors.extend(compile_result.errors);
        result.duration = start.elapsed();
        return Ok(result);
    }
    result.setup_compiled = true;

    let setup_exe = &compile_result.executable_path;

    // Step 2: Configure
    let configure_result =
        run_configure(setup_exe, package_dir, configure_flags, options.verbose).await?;
    if !configure_result.success {
        result.success = false;
        let stderr = configure_result.stderr.trim();
        if stderr.is_empty() {
            result.errors.push(format!(
                "Setup configure failed with exit code {}",
                configure_result.exit_code
            ));
        } else {
            result.errors.push(format!(
                "Setup configure failed:\n  {}",
                stderr.replace('\n', "\n  ")
            ));
        }
        result
            .errors
            .push("hint: Check that all dependencies are installed".to_string());
        result.duration = start.elapsed();
        return Ok(result);
    }
    result.configured = true;

    // Step 3: Build
    let build_result = run_build(setup_exe, package_dir, build_flags, options.verbose).await?;
    if !build_result.success {
        result.success = false;
        let stderr = build_result.stderr.trim();
        if stderr.is_empty() {
            result.errors.push(format!(
                "Setup build failed with exit code {}",
                build_result.exit_code
            ));
        } else {
            result.errors.push(format!(
                "Setup build failed:\n  {}",
                stderr.replace('\n', "\n  ")
            ));
        }
        result
            .errors
            .push("hint: Run 'hx build --verbose' for full compiler output".to_string());
        result.duration = start.elapsed();
        return Ok(result);
    }
    result.built = true;

    // Step 4: Copy (optional)
    if let Some(copy_flags) = copy_flags {
        let copy_result = run_copy(setup_exe, package_dir, copy_flags, options.verbose).await?;
        if !copy_result.success {
            result.success = false;
            let stderr = copy_result.stderr.trim();
            if stderr.is_empty() {
                result.errors.push(format!(
                    "Setup copy failed with exit code {}",
                    copy_result.exit_code
                ));
            } else {
                result.errors.push(format!(
                    "Setup copy failed:\n  {}",
                    stderr.replace('\n', "\n  ")
                ));
            }
            result
                .errors
                .push("hint: Check destination directory permissions".to_string());
            result.duration = start.elapsed();
            return Ok(result);
        }
        result.copied = true;
    }

    // Step 5: Register (optional)
    if let Some(db) = register_db {
        let register_result =
            run_register(setup_exe, package_dir, Some(db), options.verbose).await?;
        if !register_result.success {
            result.success = false;
            let stderr = register_result.stderr.trim();
            if stderr.is_empty() {
                result.errors.push(format!(
                    "Setup register failed with exit code {}",
                    register_result.exit_code
                ));
            } else {
                result.errors.push(format!(
                    "Setup register failed:\n  {}",
                    stderr.replace('\n', "\n  ")
                ));
            }
            result
                .errors
                .push("hint: Check package database permissions".to_string());
            result.duration = start.elapsed();
            return Ok(result);
        }
        result.registered = true;
    }

    result.duration = start.elapsed();
    Ok(result)
}

/// Result of a full custom build.
#[derive(Debug, Clone)]
pub struct CustomBuildResult {
    /// Whether the build succeeded overall
    pub success: bool,
    /// Total build duration
    pub duration: Duration,
    /// Whether Setup.hs was compiled successfully
    pub setup_compiled: bool,
    /// Whether configure succeeded
    pub configured: bool,
    /// Whether build succeeded
    pub built: bool,
    /// Whether copy succeeded (if requested)
    pub copied: bool,
    /// Whether register succeeded (if requested)
    pub registered: bool,
    /// Error messages
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
}

/// Parse GHC output into warnings and errors.
fn parse_ghc_output(stdout: &str, stderr: &str) -> (Vec<String>, Vec<String>) {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    let combined = format!("{}\n{}", stdout, stderr);

    for line in combined.lines() {
        let trimmed = line.trim();
        if trimmed.contains(": error:") || trimmed.contains(": Error:") {
            errors.push(trimmed.to_string());
        } else if trimmed.contains(": warning:") || trimmed.contains(": Warning:") {
            warnings.push(trimmed.to_string());
        }
    }

    (warnings, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_configure_flags_to_args() {
        let flags = ConfigureFlags {
            prefix: Some(PathBuf::from("/usr/local")),
            libdir: Some(PathBuf::from("/usr/local/lib")),
            with_ghc: Some(PathBuf::from("/usr/bin/ghc")),
            ..Default::default()
        };

        let args = flags.to_args();
        assert!(args.contains(&"--prefix=/usr/local".to_string()));
        assert!(args.contains(&"--libdir=/usr/local/lib".to_string()));
        assert!(args.contains(&"--with-ghc=/usr/bin/ghc".to_string()));
    }

    #[test]
    fn test_configure_flags_all_options() {
        let flags = ConfigureFlags {
            prefix: Some(PathBuf::from("/opt")),
            libdir: Some(PathBuf::from("/opt/lib")),
            bindir: Some(PathBuf::from("/opt/bin")),
            datadir: Some(PathBuf::from("/opt/share")),
            package_db: Some(PathBuf::from("/opt/db")),
            with_ghc: Some(PathBuf::from("/opt/ghc")),
            extra_flags: vec!["--extra1".to_string(), "--extra2".to_string()],
        };

        let args = flags.to_args();
        assert_eq!(args.len(), 8);
        assert!(args.contains(&"--prefix=/opt".to_string()));
        assert!(args.contains(&"--bindir=/opt/bin".to_string()));
        assert!(args.contains(&"--datadir=/opt/share".to_string()));
        assert!(args.contains(&"--package-db=/opt/db".to_string()));
        assert!(args.contains(&"--extra1".to_string()));
        assert!(args.contains(&"--extra2".to_string()));
    }

    #[test]
    fn test_configure_flags_empty() {
        let flags = ConfigureFlags::default();
        let args = flags.to_args();
        assert!(args.is_empty());
    }

    #[test]
    fn test_build_flags_to_args() {
        let flags = BuildFlags {
            jobs: Some(4),
            extra_flags: vec!["--verbose".to_string()],
        };

        let args = flags.to_args();
        assert!(args.contains(&"-j4".to_string()));
        assert!(args.contains(&"--verbose".to_string()));
    }

    #[test]
    fn test_build_flags_empty() {
        let flags = BuildFlags::default();
        let args = flags.to_args();
        assert!(args.is_empty());
    }

    #[test]
    fn test_build_flags_only_jobs() {
        let flags = BuildFlags {
            jobs: Some(8),
            extra_flags: Vec::new(),
        };
        let args = flags.to_args();
        assert_eq!(args, vec!["-j8".to_string()]);
    }

    #[test]
    fn test_copy_flags_to_args() {
        let flags = CopyFlags {
            destdir: Some(PathBuf::from("/tmp/install")),
            ..Default::default()
        };

        let args = flags.to_args();
        assert!(args.contains(&"--destdir=/tmp/install".to_string()));
    }

    #[test]
    fn test_copy_flags_empty() {
        let flags = CopyFlags::default();
        let args = flags.to_args();
        assert!(args.is_empty());
    }

    #[test]
    fn test_copy_flags_with_extras() {
        let flags = CopyFlags {
            destdir: Some(PathBuf::from("/staging")),
            extra_flags: vec!["--verbose".to_string()],
        };
        let args = flags.to_args();
        assert_eq!(args.len(), 2);
        assert!(args.contains(&"--destdir=/staging".to_string()));
        assert!(args.contains(&"--verbose".to_string()));
    }

    #[test]
    fn test_find_setup_file_nonexistent() {
        let result = find_setup_file(Path::new("/nonexistent/path"));
        assert!(result.is_none());
    }

    #[test]
    fn test_find_setup_file_with_setup_hs() {
        let temp = TempDir::new().unwrap();
        let setup_hs = temp.path().join("Setup.hs");
        std::fs::write(&setup_hs, "import Distribution.Simple; main = defaultMain").unwrap();

        let result = find_setup_file(temp.path());
        assert!(result.is_some());
        assert_eq!(result.unwrap(), setup_hs);
    }

    #[test]
    fn test_find_setup_file_with_setup_lhs() {
        let temp = TempDir::new().unwrap();
        let setup_lhs = temp.path().join("Setup.lhs");
        std::fs::write(
            &setup_lhs,
            "> import Distribution.Simple\n> main = defaultMain",
        )
        .unwrap();

        let result = find_setup_file(temp.path());
        assert!(result.is_some());
        assert_eq!(result.unwrap(), setup_lhs);
    }

    #[test]
    fn test_find_setup_file_prefers_hs_over_lhs() {
        let temp = TempDir::new().unwrap();
        let setup_hs = temp.path().join("Setup.hs");
        let setup_lhs = temp.path().join("Setup.lhs");
        std::fs::write(&setup_hs, "import Distribution.Simple").unwrap();
        std::fs::write(&setup_lhs, "> import Distribution.Simple").unwrap();

        let result = find_setup_file(temp.path());
        assert!(result.is_some());
        assert_eq!(result.unwrap(), setup_hs); // .hs preferred over .lhs
    }

    #[test]
    fn test_custom_setup_options_default() {
        let options = CustomSetupOptions::default();
        assert!(options.package_dbs.is_empty());
        assert!(options.setup_depends.is_empty());
        assert!(!options.verbose);
        assert_eq!(options.ghc_path, PathBuf::new());
        assert_eq!(options.build_dir, PathBuf::new());
    }

    #[test]
    fn test_setup_compile_result_fields() {
        let result = SetupCompileResult {
            executable_path: PathBuf::from("/path/to/Setup"),
            success: true,
            duration: Duration::from_millis(500),
            errors: Vec::new(),
            warnings: vec!["some warning".to_string()],
        };
        assert!(result.success);
        assert_eq!(result.executable_path, PathBuf::from("/path/to/Setup"));
        assert_eq!(result.warnings.len(), 1);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_setup_run_result_fields() {
        let result = SetupRunResult {
            success: false,
            exit_code: 1,
            stdout: "output".to_string(),
            stderr: "error".to_string(),
            duration: Duration::from_millis(100),
        };
        assert!(!result.success);
        assert_eq!(result.exit_code, 1);
        assert_eq!(result.stdout, "output");
        assert_eq!(result.stderr, "error");
    }

    #[test]
    fn test_custom_build_result_fields() {
        let result = CustomBuildResult {
            success: true,
            duration: Duration::from_secs(10),
            setup_compiled: true,
            configured: true,
            built: true,
            copied: true,
            registered: true,
            errors: Vec::new(),
            warnings: Vec::new(),
        };
        assert!(result.success);
        assert!(result.setup_compiled);
        assert!(result.configured);
        assert!(result.built);
        assert!(result.copied);
        assert!(result.registered);
    }

    #[test]
    fn test_custom_build_result_partial_failure() {
        let result = CustomBuildResult {
            success: false,
            duration: Duration::from_secs(5),
            setup_compiled: true,
            configured: true,
            built: false,
            copied: false,
            registered: false,
            errors: vec!["Build failed".to_string()],
            warnings: Vec::new(),
        };
        assert!(!result.success);
        assert!(result.setup_compiled);
        assert!(result.configured);
        assert!(!result.built);
        assert!(!result.copied);
        assert_eq!(result.errors.len(), 1);
    }

    #[test]
    fn test_parse_ghc_output_empty() {
        let (warnings, errors) = parse_ghc_output("", "");
        assert!(warnings.is_empty());
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_ghc_output_errors() {
        let stderr = r#"
src/Lib.hs:10:5: error:
    Variable not in scope: foo
src/Main.hs:20:10: error:
    Could not match expected type
"#;
        let (warnings, errors) = parse_ghc_output("", stderr);
        assert!(warnings.is_empty());
        assert_eq!(errors.len(), 2);
        assert!(errors[0].contains("error:"));
    }

    #[test]
    fn test_parse_ghc_output_warnings() {
        let stderr = r#"
src/Lib.hs:5:1: warning:
    Unused variable 'x'
src/Main.hs:15:1: warning:
    Missing type signature
"#;
        let (warnings, errors) = parse_ghc_output("", stderr);
        assert_eq!(warnings.len(), 2);
        assert!(errors.is_empty());
        assert!(warnings[0].contains("warning:"));
    }

    #[test]
    fn test_parse_ghc_output_mixed() {
        let stderr = r#"
src/Lib.hs:5:1: warning: Unused import
src/Lib.hs:10:5: error: Type mismatch
src/Main.hs:15:1: warning: Missing signature
"#;
        let (warnings, errors) = parse_ghc_output("", stderr);
        assert_eq!(warnings.len(), 2);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_parse_ghc_output_capitalized() {
        let stderr = r#"
src/Lib.hs:10:5: Error: Something wrong
src/Main.hs:15:1: Warning: Some warning
"#;
        let (warnings, errors) = parse_ghc_output("", stderr);
        assert_eq!(warnings.len(), 1);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_parse_ghc_output_combines_stdout_stderr() {
        let stdout = "src/Lib.hs:5:1: warning: From stdout";
        let stderr = "src/Main.hs:10:1: error: From stderr";
        let (warnings, errors) = parse_ghc_output(stdout, stderr);
        assert_eq!(warnings.len(), 1);
        assert_eq!(errors.len(), 1);
    }
}
