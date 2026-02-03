//! BHC REPL (interactive mode) support.

use crate::native::{BhcCompilerConfig, BhcNativeError};
use std::path::Path;
use tokio::process::Command;
use tracing::{debug, info};

/// Build argument list for a BHC REPL session.
///
/// This is extracted as a helper for testability.
pub fn build_repl_args(bhc: &BhcCompilerConfig, src_dirs: &[std::path::PathBuf]) -> Vec<String> {
    let mut args = vec!["--interactive".to_string()];

    // Add package databases
    for db in &bhc.package_dbs {
        args.push(format!("-package-db={}", db.display()));
    }

    // Add source directories
    for dir in src_dirs {
        args.push(format!("-i{}", dir.display()));
    }

    // Add profile
    args.push(format!("--profile={}", bhc.profile.as_str()));

    // Add tensor fusion if enabled
    if bhc.tensor_fusion {
        args.push("--tensor-fusion".to_string());
    }

    args
}

/// Start a BHC interactive REPL session.
///
/// This launches `bhc --interactive` with the project's package databases
/// and source directories configured.
pub async fn start_bhc_repl(
    project_root: &Path,
    bhc: &BhcCompilerConfig,
    src_dirs: &[std::path::PathBuf],
) -> Result<i32, BhcNativeError> {
    info!("Starting BHC REPL in {}", project_root.display());

    let args = build_repl_args(bhc, src_dirs);

    debug!("BHC REPL args: {:?}", args);

    let status = Command::new(&bhc.bhc_path)
        .args(&args)
        .current_dir(project_root)
        .stdin(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .status()
        .await
        .map_err(|e| BhcNativeError::Io {
            message: "failed to start BHC REPL".to_string(),
            source: e,
        })?;

    Ok(status.code().unwrap_or(1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::native::BhcCompilerConfig;
    use hx_config::BhcProfile;
    use std::path::PathBuf;

    #[test]
    fn test_repl_args_construction() {
        let bhc = BhcCompilerConfig {
            bhc_path: PathBuf::from("bhc"),
            version: "2026.2.0".to_string(),
            profile: BhcProfile::Numeric,
            package_dbs: vec![
                PathBuf::from("/cache/hx/bhc-2026.2.0/package.db"),
                PathBuf::from("/home/user/.bhc/package.db"),
            ],
            packages: Vec::new(),
            tensor_fusion: true,
            emit_kernel_report: false,
        };

        let src_dirs = vec![PathBuf::from("src"), PathBuf::from("lib")];

        let args = build_repl_args(&bhc, &src_dirs);

        assert!(args.contains(&"--interactive".to_string()));
        assert!(args.contains(&"-package-db=/cache/hx/bhc-2026.2.0/package.db".to_string()));
        assert!(args.contains(&"-package-db=/home/user/.bhc/package.db".to_string()));
        assert!(args.contains(&"-isrc".to_string()));
        assert!(args.contains(&"-ilib".to_string()));
        assert!(args.contains(&"--profile=numeric".to_string()));
        assert!(args.contains(&"--tensor-fusion".to_string()));
    }

    #[test]
    fn test_repl_args_without_tensor_fusion() {
        let bhc = BhcCompilerConfig {
            bhc_path: PathBuf::from("bhc"),
            version: "2026.2.0".to_string(),
            profile: BhcProfile::default(),
            package_dbs: Vec::new(),
            packages: Vec::new(),
            tensor_fusion: false,
            emit_kernel_report: false,
        };

        let args = build_repl_args(&bhc, &[]);

        assert!(args.contains(&"--interactive".to_string()));
        assert!(args.contains(&"--profile=default".to_string()));
        assert!(!args.contains(&"--tensor-fusion".to_string()));
    }
}
