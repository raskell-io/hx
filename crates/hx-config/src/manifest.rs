//! hx.toml manifest parsing.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

/// Error type for manifest operations.
#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("failed to read manifest: {0}")]
    ReadError(#[from] std::io::Error),

    #[error("failed to parse manifest: {0}")]
    ParseError(#[from] toml::de::Error),

    #[error("failed to serialize manifest: {0}")]
    SerializeError(#[from] toml::ser::Error),
}

/// The project kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ProjectKind {
    /// A binary/executable project
    #[default]
    Bin,
    /// A library project
    Lib,
}

impl ProjectKind {
    /// Get the string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            ProjectKind::Bin => "bin",
            ProjectKind::Lib => "lib",
        }
    }
}

/// Project configuration section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,
    /// Project kind (bin or lib)
    #[serde(default)]
    pub kind: ProjectKind,
    /// Resolver to use (currently only "cabal")
    #[serde(default = "default_resolver")]
    pub resolver: String,
}

fn default_resolver() -> String {
    "cabal".to_string()
}

/// Toolchain configuration section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ToolchainConfig {
    /// GHC version
    pub ghc: Option<String>,
    /// Cabal version
    pub cabal: Option<String>,
    /// HLS version
    pub hls: Option<String>,
}

/// Format configuration section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatConfig {
    /// Formatter to use (fourmolu or ormolu)
    #[serde(default = "default_formatter")]
    pub formatter: String,
}

fn default_formatter() -> String {
    "fourmolu".to_string()
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            formatter: default_formatter(),
        }
    }
}

/// Lint configuration section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LintConfig {
    /// Whether to enable hlint
    #[serde(default = "default_true")]
    pub hlint: bool,
}

fn default_true() -> bool {
    true
}

/// The hx.toml manifest.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Manifest {
    /// Project configuration
    pub project: ProjectConfig,

    /// Toolchain configuration
    #[serde(default)]
    pub toolchain: ToolchainConfig,

    /// Format configuration
    #[serde(default)]
    pub format: FormatConfig,

    /// Lint configuration
    #[serde(default)]
    pub lint: LintConfig,

    /// Dependencies (optional, can rely on .cabal)
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

impl Manifest {
    /// Parse a manifest from a TOML string.
    pub fn from_str(s: &str) -> Result<Self, ManifestError> {
        Ok(toml::from_str(s)?)
    }

    /// Parse a manifest from a file.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, ManifestError> {
        let content = std::fs::read_to_string(path)?;
        Self::from_str(&content)
    }

    /// Serialize the manifest to a TOML string.
    pub fn to_string(&self) -> Result<String, ManifestError> {
        Ok(toml::to_string_pretty(self)?)
    }

    /// Write the manifest to a file.
    pub fn to_file(&self, path: impl AsRef<Path>) -> Result<(), ManifestError> {
        let content = self.to_string()?;
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Create a new manifest for a project.
    pub fn new(name: impl Into<String>, kind: ProjectKind) -> Self {
        Self {
            project: ProjectConfig {
                name: name.into(),
                kind,
                resolver: default_resolver(),
            },
            toolchain: ToolchainConfig::default(),
            format: FormatConfig::default(),
            lint: LintConfig::default(),
            dependencies: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal() {
        let toml = r#"
[project]
name = "myapp"
"#;
        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.project.name, "myapp");
        assert_eq!(manifest.project.kind, ProjectKind::Bin);
    }

    #[test]
    fn test_parse_full() {
        let toml = r#"
[project]
name = "myapp"
kind = "bin"
resolver = "cabal"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
hls = "2.9.0.0"

[format]
formatter = "fourmolu"

[lint]
hlint = true
"#;
        let manifest = Manifest::from_str(toml).unwrap();
        assert_eq!(manifest.project.name, "myapp");
        assert_eq!(manifest.toolchain.ghc, Some("9.8.2".to_string()));
        assert_eq!(manifest.format.formatter, "fourmolu");
        assert!(manifest.lint.hlint);
    }

    #[test]
    fn test_roundtrip() {
        let manifest = Manifest::new("test", ProjectKind::Lib);
        let toml = manifest.to_string().unwrap();
        let parsed = Manifest::from_str(&toml).unwrap();
        assert_eq!(parsed.project.name, "test");
        assert_eq!(parsed.project.kind, ProjectKind::Lib);
    }
}
