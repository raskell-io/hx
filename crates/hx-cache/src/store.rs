//! Shared build store management.
//!
//! This module manages the shared build store, tracking which builds
//! have been completed and enabling cache-hit detection.

use crate::global_cache_dir;
use hx_core::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Metadata about a cached build.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildCacheEntry {
    /// The fingerprint of this build
    pub fingerprint: String,
    /// When this build was completed
    pub built_at: u64,
    /// Last time this entry was accessed
    pub last_accessed: u64,
    /// GHC version used
    pub ghc_version: Option<String>,
    /// Platform
    pub platform: String,
    /// Number of packages in this build
    pub package_count: usize,
    /// Project name (for display)
    pub project_name: Option<String>,
}

/// The build store index.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StoreIndex {
    /// Version of the index format
    pub version: u32,
    /// Cached build entries, keyed by fingerprint
    pub entries: HashMap<String, BuildCacheEntry>,
}

impl StoreIndex {
    /// Current index version.
    pub const VERSION: u32 = 1;

    /// Create a new empty index.
    pub fn new() -> Self {
        Self {
            version: Self::VERSION,
            entries: HashMap::new(),
        }
    }

    /// Load the store index from disk.
    pub fn load() -> Result<Self> {
        let path = store_index_path()?;
        if !path.exists() {
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(&path).map_err(|e| Error::Io {
            message: "failed to read store index".to_string(),
            path: Some(path),
            source: e,
        })?;

        let index: StoreIndex = serde_json::from_str(&content)
            .map_err(|e| Error::config(format!("invalid store index: {}", e)))?;

        Ok(index)
    }

    /// Save the store index to disk.
    pub fn save(&self) -> Result<()> {
        let path = store_index_path()?;

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| Error::Io {
                message: "failed to create store directory".to_string(),
                path: Some(parent.to_path_buf()),
                source: e,
            })?;
        }

        let content = serde_json::to_string_pretty(self)
            .map_err(|e| Error::config(format!("failed to serialize store index: {}", e)))?;

        std::fs::write(&path, content).map_err(|e| Error::Io {
            message: "failed to write store index".to_string(),
            path: Some(path),
            source: e,
        })?;

        Ok(())
    }

    /// Check if a fingerprint has a cached build.
    pub fn has_cached_build(&self, fingerprint: &str) -> bool {
        self.entries.contains_key(fingerprint)
    }

    /// Get a cached build entry.
    pub fn get(&self, fingerprint: &str) -> Option<&BuildCacheEntry> {
        self.entries.get(fingerprint)
    }

    /// Record a successful build.
    pub fn record_build(
        &mut self,
        fingerprint: String,
        ghc_version: Option<String>,
        platform: String,
        package_count: usize,
        project_name: Option<String>,
    ) {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        self.entries.insert(
            fingerprint.clone(),
            BuildCacheEntry {
                fingerprint,
                built_at: now,
                last_accessed: now,
                ghc_version,
                platform,
                package_count,
                project_name,
            },
        );
    }

    /// Mark a build as accessed (updates last_accessed time).
    pub fn touch(&mut self, fingerprint: &str) {
        if let Some(entry) = self.entries.get_mut(fingerprint) {
            entry.last_accessed = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
        }
    }

    /// Remove entries older than the given age (in seconds).
    pub fn prune_older_than(&mut self, max_age_secs: u64) -> usize {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        let cutoff = now.saturating_sub(max_age_secs);
        let before = self.entries.len();

        self.entries
            .retain(|_, entry| entry.last_accessed >= cutoff);

        before - self.entries.len()
    }

    /// Get statistics about the store.
    pub fn stats(&self) -> StoreStats {
        let mut total_packages = 0;
        let mut ghc_versions = HashMap::new();

        for entry in self.entries.values() {
            total_packages += entry.package_count;
            if let Some(ref ghc) = entry.ghc_version {
                *ghc_versions.entry(ghc.clone()).or_insert(0) += 1;
            }
        }

        StoreStats {
            entry_count: self.entries.len(),
            total_packages,
            ghc_versions,
        }
    }
}

/// Statistics about the build store.
#[derive(Debug, Clone, Default)]
pub struct StoreStats {
    /// Number of cached build entries
    pub entry_count: usize,
    /// Total number of packages across all builds
    pub total_packages: usize,
    /// Breakdown by GHC version
    pub ghc_versions: HashMap<String, usize>,
}

/// Get the path to the store index file.
fn store_index_path() -> Result<PathBuf> {
    Ok(global_cache_dir()?.join("store-index.json"))
}

/// Calculate a build fingerprint from lockfile data.
pub fn calculate_fingerprint(
    ghc_version: Option<&str>,
    cabal_version: Option<&str>,
    platform: &str,
    packages: &[(String, String)], // (name, version) pairs
) -> String {
    let mut hasher = Sha256::new();

    // Include toolchain
    if let Some(ghc) = ghc_version {
        hasher.update(format!("ghc:{}", ghc));
    }
    if let Some(cabal) = cabal_version {
        hasher.update(format!("cabal:{}", cabal));
    }

    // Include platform
    hasher.update(format!("platform:{}", platform));

    // Include packages (sorted for determinism)
    let mut sorted_packages = packages.to_vec();
    sorted_packages.sort();
    for (name, version) in sorted_packages {
        hasher.update(format!("{}@{}", name, version));
    }

    let result = hasher.finalize();
    format!("sha256:{}", hex::encode(result))
}

/// Get the size of the Cabal store on disk.
pub fn store_disk_size() -> Result<u64> {
    let store_dir = crate::cabal_store_dir()?;
    if !store_dir.exists() {
        return Ok(0);
    }
    dir_size(&store_dir)
}

fn dir_size(path: &Path) -> Result<u64> {
    let mut size = 0;

    if path.is_file() {
        return Ok(path.metadata().map(|m| m.len()).unwrap_or(0));
    }

    let entries = std::fs::read_dir(path).map_err(|e| Error::Io {
        message: "failed to read directory".to_string(),
        path: Some(path.to_path_buf()),
        source: e,
    })?;

    for entry in entries.flatten() {
        let entry_path = entry.path();
        if entry_path.is_file() {
            size += entry_path.metadata().map(|m| m.len()).unwrap_or(0);
        } else if entry_path.is_dir() {
            size += dir_size(&entry_path)?;
        }
    }

    Ok(size)
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
    fn test_fingerprint_calculation() {
        let fp1 = calculate_fingerprint(
            Some("9.8.2"),
            Some("3.12.1.0"),
            "x86_64-linux",
            &[
                ("text".to_string(), "2.1.1".to_string()),
                ("aeson".to_string(), "2.2.0.0".to_string()),
            ],
        );

        let fp2 = calculate_fingerprint(
            Some("9.8.2"),
            Some("3.12.1.0"),
            "x86_64-linux",
            &[
                ("aeson".to_string(), "2.2.0.0".to_string()),
                ("text".to_string(), "2.1.1".to_string()),
            ],
        );

        // Order shouldn't matter
        assert_eq!(fp1, fp2);

        // Different versions should produce different fingerprints
        let fp3 = calculate_fingerprint(
            Some("9.6.4"),
            Some("3.12.1.0"),
            "x86_64-linux",
            &[
                ("text".to_string(), "2.1.1".to_string()),
                ("aeson".to_string(), "2.2.0.0".to_string()),
            ],
        );
        assert_ne!(fp1, fp3);
    }

    #[test]
    fn test_store_index_operations() {
        let mut index = StoreIndex::new();

        assert!(!index.has_cached_build("sha256:abc123"));

        index.record_build(
            "sha256:abc123".to_string(),
            Some("9.8.2".to_string()),
            "x86_64-linux".to_string(),
            42,
            Some("myproject".to_string()),
        );

        assert!(index.has_cached_build("sha256:abc123"));

        let entry = index.get("sha256:abc123").unwrap();
        assert_eq!(entry.ghc_version, Some("9.8.2".to_string()));
        assert_eq!(entry.package_count, 42);

        let stats = index.stats();
        assert_eq!(stats.entry_count, 1);
        assert_eq!(stats.total_packages, 42);
    }

    #[test]
    fn test_prune_old_entries() {
        let mut index = StoreIndex::new();

        // Add an entry
        index.record_build(
            "sha256:old".to_string(),
            None,
            "x86_64-linux".to_string(),
            10,
            None,
        );

        // Manually set it to be old
        if let Some(entry) = index.entries.get_mut("sha256:old") {
            entry.last_accessed = 0; // Very old
        }

        // Add a recent entry
        index.record_build(
            "sha256:new".to_string(),
            None,
            "x86_64-linux".to_string(),
            10,
            None,
        );

        // Prune entries older than 1 day
        let pruned = index.prune_older_than(86400);
        assert_eq!(pruned, 1);
        assert!(!index.has_cached_build("sha256:old"));
        assert!(index.has_cached_build("sha256:new"));
    }
}
