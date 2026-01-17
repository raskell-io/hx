//! Toolchain manifest for tracking installed versions.

use chrono::{DateTime, Utc};
use hx_core::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use tracing::debug;

/// Manifest tracking installed toolchains.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ToolchainManifest {
    /// Schema version for future compatibility.
    #[serde(default = "default_schema_version")]
    pub schema_version: u32,
    /// Installed GHC versions.
    #[serde(default)]
    pub ghc: Vec<InstalledGhc>,
    /// Currently active GHC version (global default).
    pub active_ghc: Option<String>,
    /// Installed Cabal versions.
    #[serde(default)]
    pub cabal: Vec<InstalledCabal>,
    /// Currently active Cabal version (global default).
    pub active_cabal: Option<String>,
}

fn default_schema_version() -> u32 {
    1
}

/// An installed GHC entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledGhc {
    /// Version string (e.g., "9.8.2").
    pub version: String,
    /// Installation path.
    pub install_path: PathBuf,
    /// When this version was installed.
    pub installed_at: DateTime<Utc>,
    /// How the toolchain was installed.
    pub source: InstallSource,
}

/// An installed Cabal entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledCabal {
    /// Version string (e.g., "3.12.1.0").
    pub version: String,
    /// Installation path.
    pub install_path: PathBuf,
    /// When this version was installed.
    pub installed_at: DateTime<Utc>,
    /// How the toolchain was installed.
    pub source: InstallSource,
}

/// How the toolchain was installed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InstallSource {
    /// Direct download from haskell.org via hx.
    Direct,
    /// Installed via ghcup.
    Ghcup,
    /// System package manager or manual installation.
    System,
}

impl std::fmt::Display for InstallSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Direct => write!(f, "hx"),
            Self::Ghcup => write!(f, "ghcup"),
            Self::System => write!(f, "system"),
        }
    }
}

impl ToolchainManifest {
    /// Create a new empty manifest.
    pub fn new() -> Self {
        Self::default()
    }

    /// Load manifest from the toolchain directory.
    ///
    /// Returns an empty manifest if the file doesn't exist.
    pub fn load(toolchain_dir: &Path) -> Result<Self> {
        let manifest_path = toolchain_dir.join("manifest.json");

        if !manifest_path.exists() {
            debug!(
                "No manifest found at {}, using empty",
                manifest_path.display()
            );
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(&manifest_path).map_err(|e| Error::Io {
            message: format!("Failed to read manifest: {}", e),
            path: Some(manifest_path.clone()),
            source: e,
        })?;

        let manifest: Self = serde_json::from_str(&content).map_err(|e| {
            Error::config(format!(
                "Failed to parse manifest at {}: {}",
                manifest_path.display(),
                e
            ))
        })?;

        Ok(manifest)
    }

    /// Save manifest to the toolchain directory.
    pub fn save(&self, toolchain_dir: &Path) -> Result<()> {
        std::fs::create_dir_all(toolchain_dir).map_err(|e| Error::Io {
            message: format!("Failed to create toolchain directory: {}", e),
            path: Some(toolchain_dir.to_path_buf()),
            source: e,
        })?;

        let manifest_path = toolchain_dir.join("manifest.json");
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::config(format!("Failed to serialize manifest: {}", e)))?;

        std::fs::write(&manifest_path, content).map_err(|e| Error::Io {
            message: format!("Failed to write manifest: {}", e),
            path: Some(manifest_path),
            source: e,
        })?;

        Ok(())
    }

    /// Add an installed GHC version.
    pub fn add_ghc(&mut self, ghc: InstalledGhc) {
        // Remove any existing entry with the same version
        self.ghc.retain(|g| g.version != ghc.version);
        self.ghc.push(ghc);
        // Sort by version (newest first)
        self.ghc.sort_by(|a, b| b.version.cmp(&a.version));
    }

    /// Remove an installed GHC version.
    pub fn remove_ghc(&mut self, version: &str) -> Option<InstalledGhc> {
        if let Some(pos) = self.ghc.iter().position(|g| g.version == version) {
            let removed = self.ghc.remove(pos);
            // Clear active if we removed the active version
            if self.active_ghc.as_deref() == Some(version) {
                self.active_ghc = None;
            }
            Some(removed)
        } else {
            None
        }
    }

    /// Get an installed GHC by version.
    pub fn get_ghc(&self, version: &str) -> Option<&InstalledGhc> {
        self.ghc.iter().find(|g| g.version == version)
    }

    /// Check if a version is installed.
    pub fn is_installed(&self, version: &str) -> bool {
        self.ghc.iter().any(|g| g.version == version)
    }

    /// Get the active GHC installation.
    pub fn active(&self) -> Option<&InstalledGhc> {
        self.active_ghc.as_ref().and_then(|v| self.get_ghc(v))
    }

    /// Set the active GHC version.
    pub fn set_active(&mut self, version: &str) -> Result<()> {
        if !self.is_installed(version) {
            return Err(Error::config(format!("GHC {} is not installed", version)));
        }
        self.active_ghc = Some(version.to_string());
        Ok(())
    }

    /// Get all installed versions.
    pub fn installed_versions(&self) -> Vec<&str> {
        self.ghc.iter().map(|g| g.version.as_str()).collect()
    }

    // ---- Cabal management methods ----

    /// Add an installed Cabal version.
    pub fn add_cabal(&mut self, cabal: InstalledCabal) {
        // Remove any existing entry with the same version
        self.cabal.retain(|c| c.version != cabal.version);
        self.cabal.push(cabal);
        // Sort by version (newest first)
        self.cabal.sort_by(|a, b| b.version.cmp(&a.version));
    }

    /// Remove an installed Cabal version.
    pub fn remove_cabal(&mut self, version: &str) -> Option<InstalledCabal> {
        if let Some(pos) = self.cabal.iter().position(|c| c.version == version) {
            let removed = self.cabal.remove(pos);
            // Clear active if we removed the active version
            if self.active_cabal.as_deref() == Some(version) {
                self.active_cabal = None;
            }
            Some(removed)
        } else {
            None
        }
    }

    /// Get an installed Cabal by version.
    pub fn get_cabal(&self, version: &str) -> Option<&InstalledCabal> {
        self.cabal.iter().find(|c| c.version == version)
    }

    /// Check if a Cabal version is installed.
    pub fn is_cabal_installed(&self, version: &str) -> bool {
        self.cabal.iter().any(|c| c.version == version)
    }

    /// Get the active Cabal installation.
    pub fn active_cabal(&self) -> Option<&InstalledCabal> {
        self.active_cabal.as_ref().and_then(|v| self.get_cabal(v))
    }

    /// Set the active Cabal version.
    pub fn set_active_cabal(&mut self, version: &str) -> Result<()> {
        if !self.is_cabal_installed(version) {
            return Err(Error::config(format!("Cabal {} is not installed", version)));
        }
        self.active_cabal = Some(version.to_string());
        Ok(())
    }

    /// Get all installed Cabal versions.
    pub fn installed_cabal_versions(&self) -> Vec<&str> {
        self.cabal.iter().map(|c| c.version.as_str()).collect()
    }
}

impl InstalledGhc {
    /// Create a new InstalledGhc entry.
    pub fn new(version: impl Into<String>, install_path: impl Into<PathBuf>) -> Self {
        Self {
            version: version.into(),
            install_path: install_path.into(),
            installed_at: Utc::now(),
            source: InstallSource::Direct,
        }
    }

    /// Set the installation source.
    pub fn with_source(mut self, source: InstallSource) -> Self {
        self.source = source;
        self
    }

    /// Get the path to the ghc binary.
    pub fn ghc_path(&self) -> PathBuf {
        self.install_path.join("bin").join(ghc_binary_name())
    }

    /// Get the path to the bin directory.
    pub fn bin_dir(&self) -> PathBuf {
        self.install_path.join("bin")
    }
}

impl InstalledCabal {
    /// Create a new InstalledCabal entry.
    pub fn new(version: impl Into<String>, install_path: impl Into<PathBuf>) -> Self {
        Self {
            version: version.into(),
            install_path: install_path.into(),
            installed_at: Utc::now(),
            source: InstallSource::Direct,
        }
    }

    /// Set the installation source.
    pub fn with_source(mut self, source: InstallSource) -> Self {
        self.source = source;
        self
    }

    /// Get the path to the cabal binary.
    pub fn cabal_path(&self) -> PathBuf {
        self.install_path.join("bin").join(cabal_binary_name())
    }

    /// Get the path to the bin directory.
    pub fn bin_dir(&self) -> PathBuf {
        self.install_path.join("bin")
    }
}

/// Get the GHC binary name for the current platform.
fn ghc_binary_name() -> &'static str {
    if cfg!(windows) { "ghc.exe" } else { "ghc" }
}

/// Get the Cabal binary name for the current platform.
fn cabal_binary_name() -> &'static str {
    if cfg!(windows) { "cabal.exe" } else { "cabal" }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_manifest_new() {
        let manifest = ToolchainManifest::new();
        assert!(manifest.ghc.is_empty());
        assert!(manifest.active_ghc.is_none());
    }

    #[test]
    fn test_manifest_add_ghc() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_ghc(InstalledGhc::new("9.8.2", "/path/to/9.8.2"));
        manifest.add_ghc(InstalledGhc::new("9.6.4", "/path/to/9.6.4"));

        assert_eq!(manifest.ghc.len(), 2);
        assert!(manifest.is_installed("9.8.2"));
        assert!(manifest.is_installed("9.6.4"));
    }

    #[test]
    fn test_manifest_remove_ghc() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_ghc(InstalledGhc::new("9.8.2", "/path/to/9.8.2"));
        manifest.active_ghc = Some("9.8.2".to_string());

        let removed = manifest.remove_ghc("9.8.2");
        assert!(removed.is_some());
        assert!(!manifest.is_installed("9.8.2"));
        assert!(manifest.active_ghc.is_none()); // Should be cleared
    }

    #[test]
    fn test_manifest_set_active() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_ghc(InstalledGhc::new("9.8.2", "/path/to/9.8.2"));

        assert!(manifest.set_active("9.8.2").is_ok());
        assert_eq!(manifest.active_ghc, Some("9.8.2".to_string()));

        // Can't set active to non-installed version
        assert!(manifest.set_active("9.6.4").is_err());
    }

    #[test]
    fn test_manifest_save_load() {
        let temp = tempdir().unwrap();
        let toolchain_dir = temp.path();

        let mut manifest = ToolchainManifest::new();
        manifest.add_ghc(InstalledGhc::new("9.8.2", "/path/to/9.8.2"));
        manifest.active_ghc = Some("9.8.2".to_string());

        manifest.save(toolchain_dir).unwrap();

        let loaded = ToolchainManifest::load(toolchain_dir).unwrap();
        assert_eq!(loaded.ghc.len(), 1);
        assert_eq!(loaded.ghc[0].version, "9.8.2");
        assert_eq!(loaded.active_ghc, Some("9.8.2".to_string()));
    }

    #[test]
    fn test_installed_ghc_paths() {
        let ghc = InstalledGhc::new("9.8.2", "/home/user/.hx/toolchains/ghc/9.8.2");

        let bin_dir = ghc.bin_dir();
        assert!(bin_dir.ends_with("bin"));

        let ghc_path = ghc.ghc_path();
        assert!(ghc_path.ends_with("ghc") || ghc_path.ends_with("ghc.exe"));
    }

    // ---- Cabal tests ----

    #[test]
    fn test_manifest_add_cabal() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_cabal(InstalledCabal::new("3.12.1.0", "/path/to/3.12.1.0"));
        manifest.add_cabal(InstalledCabal::new("3.10.3.0", "/path/to/3.10.3.0"));

        assert_eq!(manifest.cabal.len(), 2);
        assert!(manifest.is_cabal_installed("3.12.1.0"));
        assert!(manifest.is_cabal_installed("3.10.3.0"));
    }

    #[test]
    fn test_manifest_remove_cabal() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_cabal(InstalledCabal::new("3.12.1.0", "/path/to/3.12.1.0"));
        manifest.active_cabal = Some("3.12.1.0".to_string());

        let removed = manifest.remove_cabal("3.12.1.0");
        assert!(removed.is_some());
        assert!(!manifest.is_cabal_installed("3.12.1.0"));
        assert!(manifest.active_cabal.is_none()); // Should be cleared
    }

    #[test]
    fn test_manifest_set_active_cabal() {
        let mut manifest = ToolchainManifest::new();
        manifest.add_cabal(InstalledCabal::new("3.12.1.0", "/path/to/3.12.1.0"));

        assert!(manifest.set_active_cabal("3.12.1.0").is_ok());
        assert_eq!(manifest.active_cabal, Some("3.12.1.0".to_string()));

        // Can't set active to non-installed version
        assert!(manifest.set_active_cabal("3.10.3.0").is_err());
    }

    #[test]
    fn test_installed_cabal_paths() {
        let cabal = InstalledCabal::new("3.12.1.0", "/home/user/.hx/toolchains/cabal/3.12.1.0");

        let bin_dir = cabal.bin_dir();
        assert!(bin_dir.ends_with("bin"));

        let cabal_path = cabal.cabal_path();
        assert!(cabal_path.ends_with("cabal") || cabal_path.ends_with("cabal.exe"));
    }
}
