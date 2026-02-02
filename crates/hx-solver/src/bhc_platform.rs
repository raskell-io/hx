//! BHC Platform curated snapshots.
//!
//! Provides embedded, curated package sets for BHC projects.
//! These are similar to Stackage snapshots but tailored for BHC.

use crate::snapshot::{Snapshot, SnapshotError, SnapshotId, SnapshotMetadata, SnapshotPackage};
use serde::Deserialize;
use std::collections::HashMap;
use tracing::debug;

/// Embedded BHC platform data.
const BHC_PLATFORM_2026_1: &str = include_str!("../data/bhc-platform-2026.1.toml");

/// Metadata for a BHC platform snapshot.
#[derive(Debug, Clone)]
pub struct BhcPlatformInfo {
    /// Platform identifier
    pub id: &'static str,
    /// BHC version
    pub bhc_version: &'static str,
    /// Compatible GHC version
    pub ghc_compat: &'static str,
    /// Recommended BHC profile
    pub recommended_profile: &'static str,
    /// Number of packages
    pub package_count: usize,
}

/// TOML structure for parsing embedded platform data.
#[derive(Debug, Deserialize)]
struct PlatformToml {
    metadata: PlatformMetadataToml,
    packages: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct PlatformMetadataToml {
    #[allow(dead_code)]
    id: String,
    bhc_version: String,
    ghc_compat: String,
    recommended_profile: String,
    #[allow(dead_code)]
    created: Option<String>,
}

/// List all available BHC platform snapshots.
pub fn list_platforms() -> Vec<BhcPlatformInfo> {
    let mut platforms = Vec::new();

    // Parse each embedded platform to get metadata
    if let Ok(parsed) = toml::from_str::<PlatformToml>(BHC_PLATFORM_2026_1) {
        platforms.push(BhcPlatformInfo {
            id: "bhc-platform-2026.1",
            bhc_version: "0.2.0",
            ghc_compat: "9.8.2",
            recommended_profile: "default",
            package_count: parsed.packages.len(),
        });
    }

    platforms
}

/// Load a BHC Platform snapshot by its identifier.
pub fn load_bhc_platform(snapshot_id: &SnapshotId) -> Result<Snapshot, SnapshotError> {
    let key = snapshot_id.key();
    debug!("Loading BHC platform snapshot: {}", key);

    let toml_content = match key.as_str() {
        "bhc-platform-2026.1" => BHC_PLATFORM_2026_1,
        _ => {
            return Err(SnapshotError::NotFound(format!(
                "BHC platform '{}' not found. Available: bhc-platform-2026.1",
                key
            )));
        }
    };

    let parsed: PlatformToml = toml::from_str(toml_content).map_err(|e| {
        SnapshotError::ParseError(format!("failed to parse BHC platform data: {}", e))
    })?;

    let mut packages = HashMap::new();
    for (name, version) in &parsed.packages {
        packages.insert(
            name.clone(),
            SnapshotPackage {
                name: name.clone(),
                version: version.clone(),
                flags: HashMap::new(),
                hidden: false,
            },
        );
    }

    let metadata = SnapshotMetadata {
        snapshot: snapshot_id.clone(),
        ghc_version: parsed.metadata.ghc_compat,
        package_count: packages.len(),
        created: parsed.metadata.created,
        bhc_version: Some(parsed.metadata.bhc_version),
        recommended_profile: Some(parsed.metadata.recommended_profile),
    };

    Ok(Snapshot { metadata, packages })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_platforms() {
        let platforms = list_platforms();
        assert!(!platforms.is_empty());
        assert_eq!(platforms[0].id, "bhc-platform-2026.1");
        assert_eq!(platforms[0].bhc_version, "0.2.0");
        assert!(platforms[0].package_count > 50);
    }

    #[test]
    fn test_load_bhc_platform() {
        let id = SnapshotId::parse("bhc-platform-2026.1").unwrap();
        let snapshot = load_bhc_platform(&id).unwrap();
        assert!(snapshot.packages.len() > 50);
        assert!(snapshot.contains("aeson"));
        assert!(snapshot.contains("servant"));
        assert!(snapshot.contains("hmatrix"));
        assert!(snapshot.contains("vector"));
        assert_eq!(snapshot.metadata.bhc_version, Some("0.2.0".to_string()));
    }

    #[test]
    fn test_load_bhc_platform_not_found() {
        let id = SnapshotId::parse("bhc-platform-2099.9").unwrap();
        assert!(load_bhc_platform(&id).is_err());
    }
}
