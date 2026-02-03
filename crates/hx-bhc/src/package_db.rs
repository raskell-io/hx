//! BHC package database management.
//!
//! This module wraps `bhc-pkg` to manage a per-version package database
//! used during native BHC compilation. Packages are registered, queried,
//! and cached through this interface.

use crate::native::BhcNativeError;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use tokio::process::Command;
use tracing::{debug, info};

/// A BHC package database backed by `bhc-pkg`.
///
/// Each database is version-specific and stored under a cache directory.
/// Operations are async because they shell out to `bhc-pkg`.
pub struct BhcPackageDb {
    /// Path to the package database directory.
    pub path: PathBuf,
    /// BHC version this database is associated with.
    pub bhc_version: String,
    /// Path to the `bhc-pkg` executable.
    pub bhc_pkg_path: PathBuf,
    /// Set of registered package identifiers.
    pub(crate) registered: HashSet<String>,
}

impl BhcPackageDb {
    /// Open or create a package database for the given BHC version.
    ///
    /// The database is stored at `<cache_dir>/bhc-<version>/package.db`.
    /// If the directory does not exist it will be created and initialized
    /// via `bhc-pkg init`.
    pub async fn open(bhc_version: &str, cache_dir: &Path) -> Result<Self, BhcNativeError> {
        let db_path = cache_dir
            .join(format!("bhc-{}", bhc_version))
            .join("package.db");

        let bhc_pkg_path = find_bhc_pkg(bhc_version).await?;

        info!(
            "Opening BHC package DB at {} (bhc-pkg: {})",
            db_path.display(),
            bhc_pkg_path.display()
        );

        let mut db = Self {
            path: db_path,
            bhc_version: bhc_version.to_string(),
            bhc_pkg_path,
            registered: HashSet::new(),
        };

        // Create the directory if needed and initialize the DB
        if !db.path.exists() {
            if let Some(parent) = db.path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| BhcNativeError::Io {
                    message: format!(
                        "failed to create package DB directory: {}",
                        parent.display()
                    ),
                    source: e,
                })?;
            }
            db.init().await?;
        }

        db.load_registered().await?;

        Ok(db)
    }

    /// Initialize the package database with `bhc-pkg init`.
    async fn init(&self) -> Result<(), BhcNativeError> {
        debug!("Initializing package DB at {}", self.path.display());

        let output = Command::new(&self.bhc_pkg_path)
            .arg("init")
            .arg(&self.path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| BhcNativeError::Io {
                message: format!(
                    "failed to run bhc-pkg init: {}",
                    self.bhc_pkg_path.display()
                ),
                source: e,
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(BhcNativeError::PackageDbError(format!(
                "bhc-pkg init failed: {}",
                stderr.trim()
            )));
        }

        Ok(())
    }

    /// Load the list of registered packages from the database.
    async fn load_registered(&mut self) -> Result<(), BhcNativeError> {
        debug!("Loading registered packages from {}", self.path.display());

        let output = Command::new(&self.bhc_pkg_path)
            .arg("list")
            .arg("--simple-output")
            .arg(format!("--package-db={}", self.path.display()))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| BhcNativeError::Io {
                message: format!(
                    "failed to run bhc-pkg list: {}",
                    self.bhc_pkg_path.display()
                ),
                source: e,
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // An empty or newly initialized DB may return non-zero; treat as empty
            debug!("bhc-pkg list returned non-zero: {}", stderr.trim());
            self.registered.clear();
            return Ok(());
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        self.registered = stdout.split_whitespace().map(|s| s.to_string()).collect();

        debug!("Loaded {} registered packages", self.registered.len());

        Ok(())
    }

    /// Register a package from a `.conf` file into the database.
    ///
    /// Runs `bhc-pkg register --force` and returns the package identifier
    /// extracted from the conf file.
    pub async fn register(&mut self, conf_file: &Path) -> Result<String, BhcNativeError> {
        info!("Registering package from {}", conf_file.display());

        let output = Command::new(&self.bhc_pkg_path)
            .arg("register")
            .arg("--force")
            .arg(format!("--package-db={}", self.path.display()))
            .arg(conf_file)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| BhcNativeError::Io {
                message: format!(
                    "failed to run bhc-pkg register: {}",
                    self.bhc_pkg_path.display()
                ),
                source: e,
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(BhcNativeError::PackageDbError(format!(
                "bhc-pkg register failed for {}: {}",
                conf_file.display(),
                stderr.trim()
            )));
        }

        // Read the conf file to extract the package id
        let content = std::fs::read_to_string(conf_file).map_err(|e| BhcNativeError::Io {
            message: format!("failed to read conf file: {}", conf_file.display()),
            source: e,
        })?;

        let package_id = extract_package_id(&content).unwrap_or_else(|| {
            conf_file
                .file_stem()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| "unknown".to_string())
        });

        self.registered.insert(package_id.clone());

        Ok(package_id)
    }

    /// Check whether a package identifier is registered.
    pub fn is_registered(&self, package_id: &str) -> bool {
        self.registered.contains(package_id)
    }

    /// Check whether a package with a given name and version is registered.
    ///
    /// Looks for entries matching `<name>-<version>` as a prefix.
    pub fn has_package(&self, name: &str, version: &str) -> bool {
        let prefix = format!("{}-{}", name, version);
        self.registered.iter().any(|id| id.starts_with(&prefix))
    }

    /// Return the set of all registered package identifiers.
    pub fn registered_packages(&self) -> &HashSet<String> {
        &self.registered
    }

    /// Recache the package database by running `bhc-pkg recache`.
    pub async fn recache(&self) -> Result<(), BhcNativeError> {
        debug!("Recaching package DB at {}", self.path.display());

        let output = Command::new(&self.bhc_pkg_path)
            .arg("recache")
            .arg(format!("--package-db={}", self.path.display()))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .map_err(|e| BhcNativeError::Io {
                message: format!(
                    "failed to run bhc-pkg recache: {}",
                    self.bhc_pkg_path.display()
                ),
                source: e,
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(BhcNativeError::PackageDbError(format!(
                "bhc-pkg recache failed: {}",
                stderr.trim()
            )));
        }

        Ok(())
    }

    /// Return the path to the package database directory.
    pub fn db_path(&self) -> &Path {
        &self.path
    }

    /// Return BHC flags that point to this package database.
    ///
    /// Returns `["-package-db", "<path>"]`.
    pub fn bhc_flags(&self) -> Vec<String> {
        vec![
            "-package-db".to_string(),
            self.path.to_string_lossy().to_string(),
        ]
    }
}

/// Find the `bhc-pkg` executable for a given BHC version.
///
/// Searches the system PATH for `bhc-pkg` (or `bhc-pkg-<version>`).
async fn find_bhc_pkg(bhc_version: &str) -> Result<PathBuf, BhcNativeError> {
    // Try version-specific name first
    let versioned = format!("bhc-pkg-{}", bhc_version);
    if let Ok(path) = which::which(&versioned) {
        debug!("Found versioned bhc-pkg: {}", path.display());
        return Ok(path);
    }

    // Fall back to unversioned
    if let Ok(path) = which::which("bhc-pkg") {
        debug!("Found bhc-pkg: {}", path.display());
        return Ok(path);
    }

    Err(BhcNativeError::BhcNotFound(format!(
        "bhc-pkg not found in PATH (tried {} and bhc-pkg)",
        versioned
    )))
}

/// Extract the `id:` field from a GHC/BHC package `.conf` file.
///
/// Looks for a line starting with `id:` and returns the value.
fn extract_package_id(content: &str) -> Option<String> {
    for line in content.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("id:") {
            let id = rest.trim();
            if !id.is_empty() {
                return Some(id.to_string());
            }
        }
    }
    None
}

/// Return the default cache directory for hx.
///
/// Uses the platform cache directory via `directories::BaseDirs` and
/// appends `hx`. Falls back to `~/.cache/hx` if unavailable.
pub fn default_cache_dir() -> PathBuf {
    directories::BaseDirs::new()
        .map(|d| d.cache_dir().to_path_buf())
        .unwrap_or_else(|| {
            directories::BaseDirs::new()
                .expect("could not determine home directory")
                .home_dir()
                .join(".cache")
        })
        .join("hx")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_package_id() {
        let content = "\
name: base
version: 4.19.0
id: base-4.19.0-abcdef1234
key: base-4.19.0-abcdef1234
";
        assert_eq!(
            extract_package_id(content),
            Some("base-4.19.0-abcdef1234".to_string())
        );
    }

    #[test]
    fn test_extract_package_id_missing() {
        let content = "\
name: base
version: 4.19.0
key: base-4.19.0-abcdef1234
";
        assert_eq!(extract_package_id(content), None);
    }

    #[test]
    fn test_extract_package_id_empty_value() {
        let content = "id:   \nname: foo\n";
        assert_eq!(extract_package_id(content), None);
    }

    #[test]
    fn test_default_cache_dir() {
        let dir = default_cache_dir();
        let dir_str = dir.to_string_lossy();
        assert!(
            dir_str.ends_with("hx"),
            "expected path ending with 'hx', got: {}",
            dir_str
        );
    }

    #[test]
    fn test_bhc_flags() {
        let db = BhcPackageDb {
            path: PathBuf::from("/tmp/test/bhc-2026.2.0/package.db"),
            bhc_version: "2026.2.0".to_string(),
            bhc_pkg_path: PathBuf::from("bhc-pkg"),
            registered: HashSet::new(),
        };

        let flags = db.bhc_flags();
        assert_eq!(flags.len(), 2);
        assert_eq!(flags[0], "-package-db");
        assert_eq!(flags[1], "/tmp/test/bhc-2026.2.0/package.db");
    }

    #[test]
    fn test_is_registered() {
        let mut registered = HashSet::new();
        registered.insert("base-4.19.0-abc123".to_string());
        registered.insert("text-2.1.0-def456".to_string());

        let db = BhcPackageDb {
            path: PathBuf::from("/tmp/test/package.db"),
            bhc_version: "2026.2.0".to_string(),
            bhc_pkg_path: PathBuf::from("bhc-pkg"),
            registered,
        };

        assert!(db.is_registered("base-4.19.0-abc123"));
        assert!(!db.is_registered("containers-0.7.0-xyz"));
    }

    #[test]
    fn test_has_package() {
        let mut registered = HashSet::new();
        registered.insert("base-4.19.0-abc123".to_string());
        registered.insert("text-2.1.0-def456".to_string());

        let db = BhcPackageDb {
            path: PathBuf::from("/tmp/test/package.db"),
            bhc_version: "2026.2.0".to_string(),
            bhc_pkg_path: PathBuf::from("bhc-pkg"),
            registered,
        };

        assert!(db.has_package("base", "4.19.0"));
        assert!(db.has_package("text", "2.1.0"));
        assert!(!db.has_package("containers", "0.7.0"));
    }
}
