//! Lockfile management for hx.
//!
//! This crate handles:
//! - Reading and writing hx.lock
//! - Converting from Cabal freeze/plan
//! - Fingerprint calculation

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::Path;
use thiserror::Error;

/// Error type for lock operations.
#[derive(Debug, Error)]
pub enum LockError {
    #[error("failed to read lockfile: {0}")]
    ReadError(#[from] std::io::Error),

    #[error("failed to parse lockfile: {0}")]
    ParseError(#[from] toml::de::Error),

    #[error("failed to serialize lockfile: {0}")]
    SerializeError(#[from] toml::ser::Error),

    #[error("lockfile version mismatch: expected {expected}, found {found}")]
    VersionMismatch { expected: u32, found: u32 },
}

/// Current lockfile format version.
pub const LOCK_VERSION: u32 = 1;

/// A locked package.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedPackage {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Source (hackage, workspace, git, etc.)
    #[serde(default = "default_source")]
    pub source: String,
    /// Content hash
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hash: Option<String>,
}

fn default_source() -> String {
    "hackage".to_string()
}

/// Toolchain section of the lockfile.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedToolchain {
    /// GHC version
    pub ghc: Option<String>,
    /// Cabal version
    pub cabal: Option<String>,
}

/// Build plan section of the lockfile.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockedPlan {
    /// Compiler ID (e.g., "ghc-9.8.2")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compiler_id: Option<String>,
    /// Target platform
    #[serde(skip_serializing_if = "Option::is_none")]
    pub platform: Option<String>,
    /// Hackage index state
    #[serde(skip_serializing_if = "Option::is_none")]
    pub index_state: Option<String>,
    /// Overall plan hash
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hash: Option<String>,
}

/// The hx.lock lockfile.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    /// Lockfile format version
    pub version: u32,
    /// When the lockfile was created/updated
    pub created_at: DateTime<Utc>,
    /// Toolchain versions
    #[serde(default)]
    pub toolchain: LockedToolchain,
    /// Build plan metadata
    #[serde(default)]
    pub plan: LockedPlan,
    /// Locked packages
    #[serde(default)]
    pub packages: Vec<LockedPackage>,
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

impl Lockfile {
    /// Create a new empty lockfile.
    pub fn new() -> Self {
        Self {
            version: LOCK_VERSION,
            created_at: Utc::now(),
            toolchain: LockedToolchain::default(),
            plan: LockedPlan::default(),
            packages: Vec::new(),
        }
    }

    /// Parse a lockfile from a TOML string.
    pub fn from_str(s: &str) -> Result<Self, LockError> {
        let lock: Lockfile = toml::from_str(s)?;
        if lock.version != LOCK_VERSION {
            return Err(LockError::VersionMismatch {
                expected: LOCK_VERSION,
                found: lock.version,
            });
        }
        Ok(lock)
    }

    /// Parse a lockfile from a file.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, LockError> {
        let content = std::fs::read_to_string(path)?;
        Self::from_str(&content)
    }

    /// Serialize the lockfile to a TOML string.
    pub fn to_string(&self) -> Result<String, LockError> {
        Ok(toml::to_string_pretty(self)?)
    }

    /// Write the lockfile to a file.
    pub fn to_file(&self, path: impl AsRef<Path>) -> Result<(), LockError> {
        let content = self.to_string()?;
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Calculate a fingerprint for the lockfile.
    pub fn fingerprint(&self) -> String {
        let mut hasher = Sha256::new();

        // Include toolchain
        if let Some(ref ghc) = self.toolchain.ghc {
            hasher.update(format!("ghc:{}", ghc));
        }
        if let Some(ref cabal) = self.toolchain.cabal {
            hasher.update(format!("cabal:{}", cabal));
        }

        // Include plan metadata
        if let Some(ref platform) = self.plan.platform {
            hasher.update(format!("platform:{}", platform));
        }
        if let Some(ref index_state) = self.plan.index_state {
            hasher.update(format!("index:{}", index_state));
        }

        // Include packages (sorted for determinism)
        let mut packages: Vec<_> = self.packages.iter().collect();
        packages.sort_by(|a, b| a.name.cmp(&b.name));
        for pkg in packages {
            hasher.update(format!("{}@{}", pkg.name, pkg.version));
        }

        let result = hasher.finalize();
        format!("sha256:{}", hex::encode(result))
    }

    /// Add a package to the lockfile.
    pub fn add_package(&mut self, pkg: LockedPackage) {
        // Remove existing package with same name
        self.packages.retain(|p| p.name != pkg.name);
        self.packages.push(pkg);
    }

    /// Set the toolchain versions.
    pub fn set_toolchain(&mut self, ghc: Option<String>, cabal: Option<String>) {
        self.toolchain.ghc = ghc;
        self.toolchain.cabal = cabal;
    }
}

/// Parse a Cabal freeze file and extract constraints.
pub fn parse_freeze_file(content: &str) -> Vec<LockedPackage> {
    let mut packages = Vec::new();

    for line in content.lines() {
        let line = line.trim();

        // Skip comments and empty lines
        if line.is_empty() || line.starts_with("--") {
            continue;
        }

        // Look for constraint lines like "constraints: pkg ==version"
        // or "             pkg ==version,"
        if let Some(constraint) = line
            .strip_prefix("constraints:")
            .or_else(|| Some(line))
            .map(|s| s.trim().trim_end_matches(','))
        {
            if let Some((name, version)) = parse_constraint(constraint) {
                packages.push(LockedPackage {
                    name,
                    version,
                    source: "hackage".to_string(),
                    hash: None,
                });
            }
        }
    }

    packages
}

fn parse_constraint(s: &str) -> Option<(String, String)> {
    // Parse "pkg ==version" or "pkg ==version,"
    let s = s.trim().trim_end_matches(',');
    let parts: Vec<&str> = s.split(" ==").collect();
    if parts.len() == 2 {
        let name = parts[0].trim();
        let version = parts[1].trim();
        if !name.is_empty() && !version.is_empty() && !name.starts_with("any.") {
            return Some((name.to_string(), version.to_string()));
        }
    }
    None
}

mod hex {
    pub fn encode(bytes: impl AsRef<[u8]>) -> String {
        bytes
            .as_ref()
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lockfile_roundtrip() {
        let mut lock = Lockfile::new();
        lock.set_toolchain(Some("9.8.2".to_string()), Some("3.12.1.0".to_string()));
        lock.add_package(LockedPackage {
            name: "text".to_string(),
            version: "2.1.1".to_string(),
            source: "hackage".to_string(),
            hash: None,
        });

        let toml = lock.to_string().unwrap();
        let parsed = Lockfile::from_str(&toml).unwrap();

        assert_eq!(parsed.toolchain.ghc, Some("9.8.2".to_string()));
        assert_eq!(parsed.packages.len(), 1);
        assert_eq!(parsed.packages[0].name, "text");
    }

    #[test]
    fn test_parse_constraint() {
        assert_eq!(
            parse_constraint("text ==2.1.1"),
            Some(("text".to_string(), "2.1.1".to_string()))
        );
        assert_eq!(
            parse_constraint("  aeson ==2.2.0.0,"),
            Some(("aeson".to_string(), "2.2.0.0".to_string()))
        );
        assert_eq!(parse_constraint("any.base ==4.19"), None);
    }
}
