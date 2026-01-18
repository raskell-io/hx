//! BHC (Basel Haskell Compiler) detection and installation.
//!
//! This module provides:
//! - BHC version detection
//! - BHC binary download and installation
//! - Version management

use std::path::PathBuf;
use std::process::Command;
use thiserror::Error;
use tracing::info;

/// Error type for BHC operations.
#[derive(Debug, Error)]
pub enum BhcError {
    #[error("BHC not found in PATH")]
    NotFound,

    #[error("failed to parse BHC version: {0}")]
    VersionParse(String),

    #[error("BHC version {0} is not known")]
    UnknownVersion(String),

    #[error("download failed: {0}")]
    Download(String),

    #[error("installation failed: {0}")]
    Install(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("network error: {0}")]
    Network(String),
}

/// Result type for BHC operations.
pub type Result<T> = std::result::Result<T, BhcError>;

/// BHC version information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BhcVersion {
    /// Major version number.
    pub major: u32,
    /// Minor version number.
    pub minor: u32,
    /// Patch version number.
    pub patch: u32,
}

impl BhcVersion {
    /// Parse a version string like "0.1.0".
    pub fn parse(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.trim().split('.').collect();
        if parts.len() != 3 {
            return Err(BhcError::VersionParse(s.to_string()));
        }

        let major = parts[0]
            .parse()
            .map_err(|_| BhcError::VersionParse(s.to_string()))?;
        let minor = parts[1]
            .parse()
            .map_err(|_| BhcError::VersionParse(s.to_string()))?;
        let patch = parts[2]
            .parse()
            .map_err(|_| BhcError::VersionParse(s.to_string()))?;

        Ok(Self {
            major,
            minor,
            patch,
        })
    }
}

impl std::fmt::Display for BhcVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Known BHC versions with their download URLs.
pub const KNOWN_BHC_VERSIONS: &[&str] = &["0.1.0", "0.2.0"];

/// Recommended BHC version.
pub const RECOMMENDED_BHC_VERSION: &str = "0.2.0";

/// Check if a version string is a known BHC version.
pub fn is_known_version(version: &str) -> bool {
    KNOWN_BHC_VERSIONS.contains(&version)
}

/// Check if a version string is valid (parseable).
pub fn is_valid_version(version: &str) -> bool {
    BhcVersion::parse(version).is_ok()
}

/// Get the list of known BHC versions.
pub fn known_versions() -> Vec<&'static str> {
    KNOWN_BHC_VERSIONS.to_vec()
}

/// Installed BHC information.
#[derive(Debug, Clone)]
pub struct InstalledBhc {
    /// Version string.
    pub version: String,
    /// Path to the BHC executable.
    pub path: PathBuf,
    /// Whether this is the active version.
    pub active: bool,
}

/// Detect BHC in the system PATH.
pub fn detect() -> Option<InstalledBhc> {
    let bhc_path = which::which("bhc").ok()?;

    // Get version
    let output = Command::new(&bhc_path).arg("--version").output().ok()?;

    if !output.status.success() {
        return None;
    }

    let version_str = String::from_utf8_lossy(&output.stdout);
    let version = parse_version_output(&version_str)?;

    Some(InstalledBhc {
        version,
        path: bhc_path,
        active: true,
    })
}

/// Parse BHC version output to extract version string.
fn parse_version_output(output: &str) -> Option<String> {
    // Try to find version pattern in output
    // Expected format: "BHC version X.Y.Z" or "bhc X.Y.Z"
    for word in output.split_whitespace() {
        if word.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            if BhcVersion::parse(word).is_ok() {
                return Some(word.to_string());
            }
        }
    }

    // Try splitting by "version"
    if let Some(rest) = output.split("version").nth(1) {
        let version = rest.trim().split_whitespace().next()?;
        if BhcVersion::parse(version).is_ok() {
            return Some(version.to_string());
        }
    }

    None
}

/// Get the default BHC installation directory.
pub fn bhc_install_dir() -> PathBuf {
    dirs::home_dir()
        .map(|home| home.join(".bhc"))
        .unwrap_or_else(|| PathBuf::from(".bhc"))
}

/// Get the path where a specific BHC version would be installed.
pub fn version_install_path(version: &str) -> PathBuf {
    bhc_install_dir().join("versions").join(version)
}

/// List installed BHC versions.
pub fn list_installed() -> Vec<InstalledBhc> {
    let install_dir = bhc_install_dir().join("versions");

    if !install_dir.exists() {
        return vec![];
    }

    let active = detect().map(|b| b.version);

    std::fs::read_dir(&install_dir)
        .ok()
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .filter(|e| e.path().is_dir())
                .filter_map(|e| {
                    let version = e.file_name().to_string_lossy().to_string();
                    let bhc_exe = if cfg!(windows) {
                        e.path().join("bin").join("bhc.exe")
                    } else {
                        e.path().join("bin").join("bhc")
                    };

                    if bhc_exe.exists() && is_valid_version(&version) {
                        Some(InstalledBhc {
                            version: version.clone(),
                            path: bhc_exe,
                            active: active.as_ref() == Some(&version),
                        })
                    } else {
                        None
                    }
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Download URL for a BHC version and platform.
pub fn bhc_download_url(version: &str, platform: &str) -> String {
    // Example URL pattern - would be replaced with actual BHC release URLs
    format!(
        "https://github.com/bhc-lang/bhc/releases/download/v{}/bhc-{}-{}.tar.gz",
        version, version, platform
    )
}

/// Get the platform identifier for the current system.
pub fn current_platform() -> String {
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    return "x86_64-linux".to_string();

    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    return "aarch64-linux".to_string();

    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    return "x86_64-darwin".to_string();

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    return "aarch64-darwin".to_string();

    #[cfg(all(target_os = "windows", target_arch = "x86_64"))]
    return "x86_64-windows".to_string();

    #[cfg(not(any(
        all(target_os = "linux", target_arch = "x86_64"),
        all(target_os = "linux", target_arch = "aarch64"),
        all(target_os = "macos", target_arch = "x86_64"),
        all(target_os = "macos", target_arch = "aarch64"),
        all(target_os = "windows", target_arch = "x86_64"),
    )))]
    return format!("{}-{}", std::env::consts::ARCH, std::env::consts::OS);
}

/// Set the active BHC version.
pub async fn set_active(version: &str) -> Result<()> {
    let install_path = version_install_path(version);
    let bhc_exe = if cfg!(windows) {
        install_path.join("bin").join("bhc.exe")
    } else {
        install_path.join("bin").join("bhc")
    };

    if !bhc_exe.exists() {
        return Err(BhcError::NotFound);
    }

    let bin_dir = bhc_install_dir().join("bin");
    std::fs::create_dir_all(&bin_dir)?;

    // Create symlink to the version's bin directory
    let link_path = if cfg!(windows) {
        bin_dir.join("bhc.exe")
    } else {
        bin_dir.join("bhc")
    };

    // Remove existing link if any
    let _ = std::fs::remove_file(&link_path);

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(&bhc_exe, &link_path)?;
    }

    #[cfg(windows)]
    {
        std::os::windows::fs::symlink_file(&bhc_exe, &link_path)?;
    }

    info!("Set BHC {} as active", version);
    Ok(())
}

/// Remove an installed BHC version.
pub async fn remove_bhc(version: &str) -> Result<()> {
    let install_path = version_install_path(version);

    if !install_path.exists() {
        return Err(BhcError::NotFound);
    }

    std::fs::remove_dir_all(&install_path)?;
    info!("Removed BHC {}", version);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_parse() {
        let v = BhcVersion::parse("0.1.0").unwrap();
        assert_eq!(v.major, 0);
        assert_eq!(v.minor, 1);
        assert_eq!(v.patch, 0);
        assert_eq!(v.to_string(), "0.1.0");
    }

    #[test]
    fn test_version_parse_invalid() {
        assert!(BhcVersion::parse("0.1").is_err());
        assert!(BhcVersion::parse("abc").is_err());
        assert!(BhcVersion::parse("0.1.0.0").is_err());
    }

    #[test]
    fn test_known_versions() {
        assert!(is_known_version("0.1.0"));
        assert!(!is_known_version("99.99.99"));
    }

    #[test]
    fn test_parse_version_output() {
        assert_eq!(
            parse_version_output("BHC version 0.1.0"),
            Some("0.1.0".to_string())
        );
        assert_eq!(parse_version_output("bhc 0.2.0"), Some("0.2.0".to_string()));
    }
}
