//! BHC manifest generation from hx.toml.
//!
//! BHC uses its own manifest format (bhc.toml). This module converts
//! hx.toml configuration to bhc.toml format for compilation.

use hx_config::Manifest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Error type for manifest operations.
#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("failed to read hx.toml: {0}")]
    ReadError(#[from] std::io::Error),

    #[error("failed to serialize bhc.toml: {0}")]
    SerializeError(#[from] toml::ser::Error),

    #[error("failed to parse: {0}")]
    ParseError(#[from] toml::de::Error),
}

/// BHC manifest format (bhc.toml).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BhcManifest {
    /// Package section.
    pub package: BhcPackage,
    /// Dependencies section.
    #[serde(default)]
    pub dependencies: HashMap<String, BhcDependency>,
    /// Build options.
    #[serde(default)]
    pub build: BhcBuildSection,
    /// Profile settings.
    #[serde(default)]
    pub profile: BhcProfileSection,
}

/// BHC package section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BhcPackage {
    /// Package name.
    pub name: String,
    /// Package version.
    #[serde(default = "default_version")]
    pub version: String,
    /// Source directories.
    #[serde(default)]
    pub src_dirs: Vec<String>,
    /// Main module (for executables).
    pub main: Option<String>,
}

fn default_version() -> String {
    "0.1.0".to_string()
}

/// BHC dependency specification.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum BhcDependency {
    /// Simple version constraint.
    Version(String),
    /// Detailed dependency.
    Detailed {
        version: Option<String>,
        features: Option<Vec<String>>,
        optional: Option<bool>,
    },
}

/// BHC build section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BhcBuildSection {
    /// Optimization level.
    pub optimization: Option<u8>,
    /// Enable tensor fusion.
    #[serde(default)]
    pub tensor_fusion: bool,
    /// Emit kernel report.
    #[serde(default)]
    pub emit_kernel_report: bool,
    /// Cross-compilation target.
    pub target: Option<String>,
}

/// BHC profile section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BhcProfileSection {
    /// Profile name.
    pub name: Option<String>,
}

impl BhcManifest {
    /// Create a new BHC manifest from an hx manifest.
    pub fn from_hx_manifest(manifest: &Manifest) -> Self {
        let bhc_config = &manifest.compiler.bhc;

        // Convert dependencies
        let dependencies: HashMap<String, BhcDependency> = manifest
            .dependencies
            .iter()
            .map(|(name, version)| (name.clone(), BhcDependency::Version(version.clone())))
            .collect();

        BhcManifest {
            package: BhcPackage {
                name: manifest.project.name.clone(),
                version: "0.1.0".to_string(),
                src_dirs: manifest.build.src_dirs.clone(),
                main: match manifest.project.kind {
                    hx_config::ProjectKind::Bin => Some("Main".to_string()),
                    hx_config::ProjectKind::Lib => None,
                },
            },
            dependencies,
            build: BhcBuildSection {
                optimization: Some(manifest.build.optimization),
                tensor_fusion: bhc_config.tensor_fusion,
                emit_kernel_report: bhc_config.emit_kernel_report,
                target: bhc_config.target.clone(),
            },
            profile: BhcProfileSection {
                name: Some(bhc_config.profile.as_str().to_string()),
            },
        }
    }

    /// Serialize to TOML string.
    pub fn to_toml(&self) -> Result<String, ManifestError> {
        Ok(toml::to_string_pretty(self)?)
    }

    /// Write to a file.
    pub fn write(&self, path: impl AsRef<Path>) -> Result<(), ManifestError> {
        let content = self.to_toml()?;
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Parse from TOML string.
    pub fn parse(content: &str) -> Result<Self, ManifestError> {
        Ok(toml::from_str(content)?)
    }

    /// Read from a file.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, ManifestError> {
        let content = std::fs::read_to_string(path)?;
        Self::parse(&content)
    }
}

/// Generate a bhc.toml file from hx.toml in the project root.
///
/// The generated file is placed in `.hx/bhc.toml` to avoid polluting
/// the project root with build system files.
pub fn generate_bhc_manifest(
    project_root: &Path,
    manifest: &Manifest,
) -> Result<PathBuf, ManifestError> {
    let hx_dir = project_root.join(".hx");
    std::fs::create_dir_all(&hx_dir)?;

    let bhc_manifest = BhcManifest::from_hx_manifest(manifest);
    let bhc_path = hx_dir.join("bhc.toml");

    bhc_manifest.write(&bhc_path)?;

    Ok(bhc_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hx_config::{BhcConfig, BhcProfile, ProjectKind};

    #[test]
    fn test_convert_hx_manifest() {
        let mut manifest = Manifest::new("test-project", ProjectKind::Bin);
        manifest.dependencies.insert("base".to_string(), ">=4.18".to_string());
        manifest.dependencies.insert("text".to_string(), ">=2.0".to_string());
        manifest.compiler.bhc = BhcConfig {
            profile: BhcProfile::Numeric,
            emit_kernel_report: true,
            tensor_fusion: true,
            target: Some("aarch64-linux-gnu".to_string()),
        };

        let bhc_manifest = BhcManifest::from_hx_manifest(&manifest);

        assert_eq!(bhc_manifest.package.name, "test-project");
        assert_eq!(bhc_manifest.package.main, Some("Main".to_string()));
        assert_eq!(bhc_manifest.dependencies.len(), 2);
        assert!(bhc_manifest.build.tensor_fusion);
        assert!(bhc_manifest.build.emit_kernel_report);
        assert_eq!(bhc_manifest.build.target, Some("aarch64-linux-gnu".to_string()));
        assert_eq!(bhc_manifest.profile.name, Some("numeric".to_string()));
    }

    #[test]
    fn test_bhc_manifest_toml_roundtrip() {
        let manifest = BhcManifest {
            package: BhcPackage {
                name: "test".to_string(),
                version: "0.1.0".to_string(),
                src_dirs: vec!["src".to_string()],
                main: Some("Main".to_string()),
            },
            dependencies: HashMap::new(),
            build: BhcBuildSection::default(),
            profile: BhcProfileSection::default(),
        };

        let toml_str = manifest.to_toml().unwrap();
        let parsed = BhcManifest::parse(&toml_str).unwrap();

        assert_eq!(parsed.package.name, "test");
        assert_eq!(parsed.package.version, "0.1.0");
    }
}
