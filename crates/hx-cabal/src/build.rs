//! Cabal build operations.

use hx_cache::{cabal_store_dir, ensure_dir};
use hx_core::{CommandOutput, CommandRunner, Error, Result};
use hx_ui::{Output, Spinner};
use std::path::PathBuf;
use std::time::Duration;
use tracing::{debug, info};

/// Options for building.
#[derive(Debug, Clone, Default)]
pub struct BuildOptions {
    /// Build in release mode
    pub release: bool,
    /// Number of parallel jobs
    pub jobs: Option<usize>,
    /// Target triple
    pub target: Option<String>,
    /// Verbose output
    pub verbose: bool,
}

/// Result of a build operation.
#[derive(Debug)]
pub struct BuildResult {
    /// Whether the build succeeded
    pub success: bool,
    /// Build duration
    pub duration: Duration,
    /// Any errors encountered
    pub errors: Vec<String>,
    /// Warnings
    pub warnings: Vec<String>,
}

/// Run cabal build.
pub async fn build(
    project_root: &PathBuf,
    build_dir: &PathBuf,
    options: &BuildOptions,
    output: &Output,
) -> Result<BuildResult> {
    let store_dir = cabal_store_dir()?;
    ensure_dir(&store_dir)?;
    ensure_dir(build_dir)?;

    let mut args = vec![
        "build".to_string(),
        format!("--builddir={}", build_dir.display()),
        format!("--store-dir={}", store_dir.display()),
    ];

    if options.release {
        args.push("-O2".to_string());
    }

    if let Some(jobs) = options.jobs {
        args.push(format!("-j{}", jobs));
    }

    info!("Running cabal build in {}", project_root.display());
    debug!("Args: {:?}", args);

    let spinner = if !options.verbose {
        Some(Spinner::new("Building..."))
    } else {
        None
    };

    let runner = CommandRunner::new().with_working_dir(project_root);
    let cmd_output = runner.run("cabal", args.iter().map(|s| s.as_str())).await?;

    let result = parse_build_output(&cmd_output);

    if let Some(spinner) = spinner {
        if result.success {
            spinner.finish_success(format!("Built in {}", format_duration(result.duration)));
        } else {
            spinner.finish_error("Build failed");
        }
    }

    if options.verbose || !result.success {
        if !cmd_output.stdout.is_empty() {
            output.verbose(&cmd_output.stdout);
        }
        if !cmd_output.stderr.is_empty() {
            if result.success {
                output.verbose(&cmd_output.stderr);
            } else {
                eprintln!("{}", cmd_output.stderr);
            }
        }
    }

    if !result.success {
        return Err(Error::BuildFailed {
            errors: result.errors.clone(),
            fixes: vec![],
        });
    }

    Ok(result)
}

/// Run cabal test.
pub async fn test(
    project_root: &PathBuf,
    build_dir: &PathBuf,
    pattern: Option<&str>,
    output: &Output,
) -> Result<BuildResult> {
    let store_dir = cabal_store_dir()?;

    let mut args = vec![
        "test".to_string(),
        format!("--builddir={}", build_dir.display()),
        format!("--store-dir={}", store_dir.display()),
    ];

    if let Some(p) = pattern {
        args.push(format!("--test-option=--pattern={}", p));
    }

    info!("Running cabal test in {}", project_root.display());

    let spinner = Spinner::new("Testing...");

    let runner = CommandRunner::new().with_working_dir(project_root);
    let cmd_output = runner.run("cabal", args.iter().map(|s| s.as_str())).await?;

    let result = parse_build_output(&cmd_output);

    if result.success {
        spinner.finish_success(format!(
            "Tests passed in {}",
            format_duration(result.duration)
        ));
    } else {
        spinner.finish_error("Tests failed");
    }

    if !cmd_output.stdout.is_empty() {
        output.info(&cmd_output.stdout);
    }

    if !result.success {
        return Err(Error::BuildFailed {
            errors: result.errors.clone(),
            fixes: vec![],
        });
    }

    Ok(result)
}

/// Run cabal run.
pub async fn run(
    project_root: &PathBuf,
    build_dir: &PathBuf,
    args: &[String],
    _output: &Output,
) -> Result<i32> {
    let store_dir = cabal_store_dir()?;

    let mut cmd_args = vec![
        "run".to_string(),
        format!("--builddir={}", build_dir.display()),
        format!("--store-dir={}", store_dir.display()),
    ];

    // Add -- to separate cabal args from program args
    if !args.is_empty() {
        cmd_args.push("--".to_string());
        cmd_args.extend(args.iter().cloned());
    }

    info!("Running cabal run in {}", project_root.display());

    let runner = CommandRunner::new().with_working_dir(project_root);
    let cmd_output = runner
        .run("cabal", cmd_args.iter().map(|s| s.as_str()))
        .await?;

    // Print output directly for run
    if !cmd_output.stdout.is_empty() {
        print!("{}", cmd_output.stdout);
    }
    if !cmd_output.stderr.is_empty() {
        eprint!("{}", cmd_output.stderr);
    }

    Ok(cmd_output.exit_code)
}

/// Run cabal repl.
pub async fn repl(project_root: &PathBuf, build_dir: &PathBuf) -> Result<i32> {
    let store_dir = cabal_store_dir()?;

    let args = vec![
        "repl".to_string(),
        format!("--builddir={}", build_dir.display()),
        format!("--store-dir={}", store_dir.display()),
    ];

    info!("Running cabal repl in {}", project_root.display());

    // For repl, we need to run interactively
    let status = std::process::Command::new("cabal")
        .args(args.iter().map(|s| s.as_str()))
        .current_dir(project_root)
        .status()
        .map_err(|e| Error::Io {
            message: "failed to run cabal repl".to_string(),
            path: None,
            source: e,
        })?;

    Ok(status.code().unwrap_or(1))
}

fn parse_build_output(output: &CommandOutput) -> BuildResult {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Parse stderr for errors and warnings
    for line in output.stderr.lines() {
        if line.contains("error:") || line.contains("Error:") {
            errors.push(line.to_string());
        } else if line.contains("warning:") || line.contains("Warning:") {
            warnings.push(line.to_string());
        }
    }

    BuildResult {
        success: output.success(),
        duration: output.duration,
        errors,
        warnings,
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
