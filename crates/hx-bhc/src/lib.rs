//! BHC (Basel Haskell Compiler) backend for hx.
//!
//! This crate provides:
//! - BHC compiler backend implementation
//! - BHC-specific build options and diagnostics
//! - bhc.toml manifest generation

pub mod build;
pub mod builtin_packages;
pub mod compile;
pub mod diagnostics;
pub mod full_native;
pub mod manifest;
pub mod native;
pub mod native_builder;
pub mod package_build;
pub mod package_db;
pub mod repl;

use async_trait::async_trait;
use hx_compiler::{
    BuildOptions as CompilerBuildOptions, BuildResult as CompilerBuildResult,
    CheckOptions as CompilerCheckOptions, CheckResult as CompilerCheckResult, CompilerBackend,
    CompilerError, CompilerStatus, Diagnostic, Result as CompilerResult,
    RunOptions as CompilerRunOptions, RunResult as CompilerRunResult,
};
use hx_config::BhcConfig;
use hx_ui::Output;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;
use tracing::{debug, info};

pub use build::BhcBuildOptions;
pub use diagnostics::parse_bhc_output;
pub use manifest::{BhcManifest, generate_bhc_manifest};

/// BHC compiler backend.
///
/// This backend invokes the BHC compiler directly for builds.
pub struct BhcBackend {
    /// Path to the BHC executable.
    bhc_path: Option<PathBuf>,
    /// BHC-specific configuration.
    config: BhcConfig,
}

impl BhcBackend {
    /// Create a new BHC backend.
    pub fn new() -> Self {
        Self {
            bhc_path: None,
            config: BhcConfig::default(),
        }
    }

    /// Set the path to the BHC executable.
    pub fn with_bhc_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.bhc_path = Some(path.into());
        self
    }

    /// Set the BHC configuration.
    pub fn with_config(mut self, config: BhcConfig) -> Self {
        self.config = config;
        self
    }

    /// Get the BHC executable path.
    pub fn bhc_cmd(&self) -> String {
        self.bhc_path
            .as_ref()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| "bhc".to_string())
    }

    /// Detect BHC version by running `bhc --version`.
    async fn detect_bhc_version(&self) -> CompilerResult<String> {
        let bhc_cmd = self.bhc_cmd();

        let output = Command::new(&bhc_cmd).arg("--version").output()?;

        if !output.status.success() {
            return Err(CompilerError::NotFound {
                name: "bhc".to_string(),
            });
        }

        let version_str = String::from_utf8_lossy(&output.stdout);
        // Parse version from "BHC version X.Y.Z" or similar
        if let Some(version) = version_str.split("version").nth(1) {
            Ok(version.trim().to_string())
        } else {
            // Try to find version number
            for word in version_str.split_whitespace() {
                if word.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                    return Ok(word.to_string());
                }
            }
            Ok(version_str.trim().to_string())
        }
    }

    /// Build command-line arguments for BHC test.
    pub fn test_args(
        &self,
        pattern: Option<&str>,
        package: Option<&str>,
        target: Option<&str>,
    ) -> Vec<String> {
        let mut args = vec!["test".to_string()];

        if let Some(pkg) = package {
            args.push(pkg.to_string());
        }

        if let Some(pat) = pattern {
            args.push(format!("--pattern={}", pat));
        }

        if let Some(tgt) = target {
            args.push(format!("--target={}", tgt));
        } else if let Some(ref tgt) = self.config.target {
            args.push(format!("--target={}", tgt));
        }

        args
    }

    /// Build command-line arguments for BHC.
    fn build_args(&self, options: &CompilerBuildOptions) -> Vec<String> {
        let mut args = vec!["build".to_string()];

        // Add profile
        args.push(format!("--profile={}", self.config.profile.as_str()));

        // Add optimization level
        let opt_level = if options.release {
            2
        } else {
            options.optimization.unwrap_or(1)
        };
        args.push(format!("-O{}", opt_level));

        // Add target if specified
        if let Some(ref target) = options.target {
            args.push(format!("--target={}", target));
        } else if let Some(ref target) = self.config.target {
            args.push(format!("--target={}", target));
        }

        // Add parallelism
        if let Some(jobs) = options.jobs {
            args.push(format!("-j{}", jobs));
        }

        // Add BHC-specific options
        if self.config.emit_kernel_report {
            args.push("--emit-kernel-report".to_string());
        }

        if self.config.tensor_fusion {
            args.push("--tensor-fusion".to_string());
        }

        // Add extra flags
        args.extend(options.extra_flags.iter().cloned());

        args
    }
}

impl Default for BhcBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl CompilerBackend for BhcBackend {
    fn name(&self) -> &str {
        "bhc"
    }

    fn description(&self) -> &str {
        "BHC (Basel Haskell Compiler) - alternative compiler with tensor optimizations"
    }

    async fn detect(&self) -> CompilerResult<CompilerStatus> {
        match self.detect_bhc_version().await {
            Ok(version) => {
                let path = self
                    .bhc_path
                    .clone()
                    .unwrap_or_else(|| PathBuf::from("bhc"));
                Ok(CompilerStatus::Available { version, path })
            }
            Err(_) => Ok(CompilerStatus::NotInstalled),
        }
    }

    async fn version(&self) -> CompilerResult<String> {
        self.detect_bhc_version().await
    }

    async fn build(
        &self,
        project_root: &Path,
        options: &CompilerBuildOptions,
        output: &Output,
    ) -> CompilerResult<CompilerBuildResult> {
        let bhc_cmd = self.bhc_cmd();
        let args = self.build_args(options);

        info!("Running BHC build in {}", project_root.display());
        debug!("Args: {:?}", args);

        output.status(
            "Building",
            &format!("with BHC ({})", self.config.profile.as_str()),
        );

        let start = Instant::now();

        let cmd_output = Command::new(&bhc_cmd)
            .args(&args)
            .current_dir(project_root)
            .output()?;

        let duration = start.elapsed();
        let success = cmd_output.status.success();

        let stdout = String::from_utf8_lossy(&cmd_output.stdout);
        let stderr = String::from_utf8_lossy(&cmd_output.stderr);

        // Parse diagnostics from output
        let diagnostics = diagnostics::parse_bhc_output(&stderr);
        let (warnings, errors): (Vec<_>, Vec<_>) = diagnostics
            .into_iter()
            .partition(|d| d.severity == hx_compiler::DiagnosticSeverity::Warning);

        if options.verbose || !success {
            if !stdout.is_empty() {
                output.verbose(&stdout);
            }
            if !stderr.is_empty() {
                if success {
                    output.verbose(&stderr);
                } else {
                    eprintln!("{}", stderr);
                }
            }
        }

        if success {
            output.status(
                "Finished",
                &format!("BHC build in {:.2}s", duration.as_secs_f64()),
            );
        } else {
            return Err(CompilerError::BuildFailed {
                error_count: errors.len(),
                errors: errors.iter().map(|e| e.message.clone()).collect(),
            });
        }

        Ok(CompilerBuildResult {
            success,
            duration,
            modules_compiled: 0, // Would need to parse BHC output
            modules_skipped: 0,
            executable: None,
            library: None,
            warnings,
            errors,
        })
    }

    async fn check(
        &self,
        project_root: &Path,
        options: &CompilerCheckOptions,
        output: &Output,
    ) -> CompilerResult<CompilerCheckResult> {
        let bhc_cmd = self.bhc_cmd();
        let mut args = vec!["check".to_string()];

        if let Some(ref pkg) = options.package {
            args.push(pkg.clone());
        }

        args.extend(options.extra_flags.iter().cloned());

        info!("Running BHC check in {}", project_root.display());
        debug!("Args: {:?}", args);

        output.status("Checking", "with BHC");

        let start = Instant::now();

        let cmd_output = Command::new(&bhc_cmd)
            .args(&args)
            .current_dir(project_root)
            .output()?;

        let duration = start.elapsed();
        let success = cmd_output.status.success();

        let stderr = String::from_utf8_lossy(&cmd_output.stderr);

        // Parse diagnostics from output
        let diagnostics = diagnostics::parse_bhc_output(&stderr);
        let (warnings, errors): (Vec<_>, Vec<_>) = diagnostics
            .into_iter()
            .partition(|d| d.severity == hx_compiler::DiagnosticSeverity::Warning);

        if !success {
            return Err(CompilerError::CheckFailed {
                error_count: errors.len(),
                errors: errors.iter().map(|e| e.message.clone()).collect(),
            });
        }

        Ok(CompilerCheckResult {
            success,
            duration,
            modules_checked: 0,
            warnings,
            errors,
        })
    }

    async fn run(
        &self,
        project_root: &Path,
        options: &CompilerRunOptions,
        output: &Output,
    ) -> CompilerResult<CompilerRunResult> {
        let bhc_cmd = self.bhc_cmd();
        let mut args = vec!["run".to_string()];

        if let Some(ref pkg) = options.package {
            args.push(pkg.clone());
        }

        // Add program arguments after --
        if !options.args.is_empty() {
            args.push("--".to_string());
            args.extend(options.args.iter().cloned());
        }

        info!("Running BHC run in {}", project_root.display());
        debug!("Args: {:?}", args);

        output.status("Running", "with BHC");

        let start = Instant::now();

        let status = Command::new(&bhc_cmd)
            .args(&args)
            .current_dir(project_root)
            .status()?;

        let duration = start.elapsed();
        let exit_code = status.code().unwrap_or(1);

        if exit_code != 0 {
            return Err(CompilerError::RunFailed {
                message: format!("BHC run exited with code {}", exit_code),
            });
        }

        Ok(CompilerRunResult {
            exit_code,
            duration,
        })
    }

    fn parse_diagnostics(&self, raw_output: &str) -> Vec<Diagnostic> {
        diagnostics::parse_bhc_output(raw_output)
    }

    fn supported_targets(&self) -> Vec<&str> {
        vec![
            "x86_64-linux-gnu",
            "aarch64-linux-gnu",
            "x86_64-apple-darwin",
            "aarch64-apple-darwin",
            // BHC-specific targets
            "wasm32-wasi",
            "riscv64-linux-gnu",
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hx_config::BhcProfile;

    #[test]
    fn test_build_args_default() {
        let backend = BhcBackend::new();
        let options = CompilerBuildOptions::default();
        let args = backend.build_args(&options);

        assert!(args.contains(&"build".to_string()));
        assert!(args.contains(&"--profile=default".to_string()));
        assert!(args.contains(&"-O1".to_string()));
    }

    #[test]
    fn test_build_args_release() {
        let backend = BhcBackend::new();
        let options = CompilerBuildOptions {
            release: true,
            ..Default::default()
        };
        let args = backend.build_args(&options);

        assert!(args.contains(&"-O2".to_string()));
    }

    #[test]
    fn test_test_args_default() {
        let backend = BhcBackend::new();
        let args = backend.test_args(None, None, None);

        assert_eq!(args, vec!["test".to_string()]);
    }

    #[test]
    fn test_test_args_with_pattern() {
        let backend = BhcBackend::new();
        let args = backend.test_args(Some("MyModule"), Some("my-pkg"), None);

        assert!(args.contains(&"test".to_string()));
        assert!(args.contains(&"my-pkg".to_string()));
        assert!(args.contains(&"--pattern=MyModule".to_string()));
    }

    #[test]
    fn test_build_args_with_config() {
        let config = BhcConfig {
            profile: BhcProfile::Numeric,
            emit_kernel_report: true,
            tensor_fusion: true,
            target: Some("aarch64-linux-gnu".to_string()),
        };
        let backend = BhcBackend::new().with_config(config);
        let options = CompilerBuildOptions::default();
        let args = backend.build_args(&options);

        assert!(args.contains(&"--profile=numeric".to_string()));
        assert!(args.contains(&"--emit-kernel-report".to_string()));
        assert!(args.contains(&"--tensor-fusion".to_string()));
        assert!(args.contains(&"--target=aarch64-linux-gnu".to_string()));
    }
}
