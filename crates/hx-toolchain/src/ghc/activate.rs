//! GHC version activation and resolution.
//!
//! Provides utilities for resolving which GHC to use and setting up
//! the environment for builds.

use crate::ghc::{InstalledGhc, ToolchainManifest};
use hx_cache::{toolchain_bin_dir, toolchain_dir};
use hx_core::{Error, Result};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, info};

/// Source of a resolved GHC version.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GhcSource {
    /// GHC installed by hx (in ~/.hx/toolchains).
    HxManaged,
    /// GHC from ghcup.
    Ghcup,
    /// GHC from system PATH.
    System,
}

/// A resolved GHC installation.
#[derive(Debug, Clone)]
pub struct ResolvedGhc {
    /// Version string.
    pub version: String,
    /// Path to the bin directory containing ghc.
    pub bin_dir: PathBuf,
    /// Source of this GHC.
    pub source: GhcSource,
}

impl ResolvedGhc {
    /// Get the path to the ghc binary.
    pub fn ghc_path(&self) -> PathBuf {
        self.bin_dir.join(ghc_binary_name())
    }

    /// Get the path to ghc-pkg.
    pub fn ghc_pkg_path(&self) -> PathBuf {
        self.bin_dir.join(ghc_pkg_binary_name())
    }
}

/// Version resolution priority.
#[derive(Debug, Clone, Default)]
pub struct ResolutionConfig {
    /// Explicit version from CLI flag.
    pub cli_version: Option<String>,
    /// Version from project hx.toml.
    pub project_version: Option<String>,
}

/// Resolve which GHC to use based on priority:
/// 1. CLI flag version
/// 2. Project hx.toml version
/// 3. hx-managed active version
/// 4. System PATH
pub fn resolve_ghc(config: &ResolutionConfig) -> Result<Option<ResolvedGhc>> {
    let tc_dir = toolchain_dir()?;

    // Check CLI override first
    if let Some(ref version) = config.cli_version {
        debug!("Resolving GHC from CLI flag: {}", version);
        if let Some(resolved) = resolve_version_from_hx(version, &tc_dir)? {
            return Ok(Some(resolved));
        }
        // CLI flag specified but version not found in hx-managed
        return Err(Error::config(format!(
            "GHC {} not found. Install with: hx toolchain install {}",
            version, version
        )));
    }

    // Check project config
    if let Some(ref version) = config.project_version {
        debug!("Resolving GHC from project config: {}", version);
        if let Some(resolved) = resolve_version_from_hx(version, &tc_dir)? {
            return Ok(Some(resolved));
        }
        // Project specifies a version but it's not installed
        return Err(Error::config(format!(
            "Project requires GHC {} but it's not installed. Install with: hx toolchain install {}",
            version, version
        )));
    }

    // Check hx-managed active version
    let manifest = ToolchainManifest::load(&tc_dir)?;
    if let Some(active) = manifest.active() {
        debug!("Using hx-managed active GHC: {}", active.version);
        return Ok(Some(ResolvedGhc {
            version: active.version.clone(),
            bin_dir: active.bin_dir(),
            source: GhcSource::HxManaged,
        }));
    }

    // Fall back to system PATH
    if let Some(resolved) = resolve_from_path()? {
        debug!("Using system GHC: {}", resolved.version);
        return Ok(Some(resolved));
    }

    Ok(None)
}

/// Try to resolve a specific version from hx-managed installations.
fn resolve_version_from_hx(version: &str, tc_dir: &Path) -> Result<Option<ResolvedGhc>> {
    let manifest = ToolchainManifest::load(tc_dir)?;

    // Check for exact match
    if let Some(installed) = manifest.get_ghc(version) {
        return Ok(Some(ResolvedGhc {
            version: installed.version.clone(),
            bin_dir: installed.bin_dir(),
            source: GhcSource::HxManaged,
        }));
    }

    // Check for major.minor match (e.g., "9.8" matches "9.8.2")
    for installed in &manifest.ghc {
        if installed.version.starts_with(version)
            && installed.version.chars().nth(version.len()) == Some('.')
        {
            return Ok(Some(ResolvedGhc {
                version: installed.version.clone(),
                bin_dir: installed.bin_dir(),
                source: GhcSource::HxManaged,
            }));
        }
    }

    Ok(None)
}

/// Try to find GHC in system PATH.
fn resolve_from_path() -> Result<Option<ResolvedGhc>> {
    let ghc_path = match which::which("ghc") {
        Ok(p) => p,
        Err(_) => return Ok(None),
    };

    // Get version from ghc --numeric-version
    let output = std::process::Command::new(&ghc_path)
        .arg("--numeric-version")
        .output()
        .map_err(|e| Error::Io {
            message: "Failed to run ghc --numeric-version".into(),
            path: Some(ghc_path.clone()),
            source: e,
        })?;

    if !output.status.success() {
        return Ok(None);
    }

    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let bin_dir = ghc_path.parent().unwrap_or(&ghc_path).to_path_buf();

    // Determine source (ghcup or system)
    let source = if bin_dir.to_string_lossy().contains(".ghcup") {
        GhcSource::Ghcup
    } else {
        GhcSource::System
    };

    Ok(Some(ResolvedGhc {
        version,
        bin_dir,
        source,
    }))
}

/// Get the PATH string with hx-managed GHC bin directory prepended.
pub fn get_path_with_ghc(ghc: &ResolvedGhc) -> String {
    let current_path = std::env::var("PATH").unwrap_or_default();
    let bin_dir = ghc.bin_dir.to_string_lossy();

    #[cfg(windows)]
    let separator = ";";
    #[cfg(not(windows))]
    let separator = ":";

    format!("{}{}{}", bin_dir, separator, current_path)
}

/// Create symlinks in ~/.hx/bin for the active GHC.
///
/// This allows users to add ~/.hx/bin to their PATH for easy access.
pub fn create_symlinks(ghc: &InstalledGhc) -> Result<()> {
    let bin_dir = toolchain_bin_dir()?;
    fs::create_dir_all(&bin_dir).map_err(|e| Error::Io {
        message: "Failed to create bin directory".into(),
        path: Some(bin_dir.clone()),
        source: e,
    })?;

    let tools = ["ghc", "ghci", "ghc-pkg", "runghc", "runhaskell", "haddock"];

    for tool in tools {
        let source = ghc.bin_dir().join(tool_binary_name(tool));
        let target = bin_dir.join(tool_binary_name(tool));

        if source.exists() {
            // Remove existing symlink/file
            let _ = fs::remove_file(&target);

            #[cfg(unix)]
            {
                std::os::unix::fs::symlink(&source, &target).map_err(|e| Error::Io {
                    message: format!("Failed to create symlink for {}", tool),
                    path: Some(target.clone()),
                    source: e,
                })?;
            }

            #[cfg(windows)]
            {
                // On Windows, create a .cmd wrapper instead of symlink
                let content = format!("@echo off\n\"{}\" %*", source.display());
                let target_cmd = bin_dir.join(format!("{}.cmd", tool));
                fs::write(&target_cmd, content).map_err(|e| Error::Io {
                    message: format!("Failed to create wrapper for {}", tool),
                    path: Some(target_cmd),
                    source: e,
                })?;
            }

            debug!(
                "Created symlink: {} -> {}",
                target.display(),
                source.display()
            );
        }
    }

    info!("Symlinks created in {}", bin_dir.display());
    Ok(())
}

/// Remove symlinks from ~/.hx/bin.
pub fn remove_symlinks() -> Result<()> {
    let bin_dir = toolchain_bin_dir()?;

    if !bin_dir.exists() {
        return Ok(());
    }

    let tools = ["ghc", "ghci", "ghc-pkg", "runghc", "runhaskell", "haddock"];

    for tool in tools {
        let target = bin_dir.join(tool_binary_name(tool));
        if target.exists() || target.is_symlink() {
            let _ = fs::remove_file(&target);
        }

        #[cfg(windows)]
        {
            let target_cmd = bin_dir.join(format!("{}.cmd", tool));
            let _ = fs::remove_file(&target_cmd);
        }
    }

    Ok(())
}

fn ghc_binary_name() -> &'static str {
    if cfg!(windows) { "ghc.exe" } else { "ghc" }
}

fn ghc_pkg_binary_name() -> &'static str {
    if cfg!(windows) {
        "ghc-pkg.exe"
    } else {
        "ghc-pkg"
    }
}

fn tool_binary_name(name: &str) -> String {
    if cfg!(windows) {
        format!("{}.exe", name)
    } else {
        name.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolution_config_default() {
        let config = ResolutionConfig::default();
        assert!(config.cli_version.is_none());
        assert!(config.project_version.is_none());
    }

    #[test]
    fn test_ghc_source_equality() {
        assert_eq!(GhcSource::HxManaged, GhcSource::HxManaged);
        assert_ne!(GhcSource::HxManaged, GhcSource::Ghcup);
    }

    #[test]
    fn test_resolved_ghc_paths() {
        let resolved = ResolvedGhc {
            version: "9.8.2".to_string(),
            bin_dir: PathBuf::from("/path/to/ghc/bin"),
            source: GhcSource::HxManaged,
        };

        let ghc_path = resolved.ghc_path();
        assert!(ghc_path.ends_with("ghc") || ghc_path.ends_with("ghc.exe"));

        let pkg_path = resolved.ghc_pkg_path();
        assert!(pkg_path.ends_with("ghc-pkg") || pkg_path.ends_with("ghc-pkg.exe"));
    }

    #[test]
    fn test_get_path_with_ghc() {
        let resolved = ResolvedGhc {
            version: "9.8.2".to_string(),
            bin_dir: PathBuf::from("/path/to/ghc/bin"),
            source: GhcSource::HxManaged,
        };

        let path = get_path_with_ghc(&resolved);
        assert!(path.starts_with("/path/to/ghc/bin"));
    }
}
