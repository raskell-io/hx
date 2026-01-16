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

/// Plugin hook configuration section.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct PluginHookConfig {
    /// Scripts to run before build.
    pub pre_build: Vec<String>,

    /// Scripts to run after build.
    pub post_build: Vec<String>,

    /// Scripts to run before tests.
    pub pre_test: Vec<String>,

    /// Scripts to run after tests.
    pub post_test: Vec<String>,

    /// Scripts to run before run command.
    pub pre_run: Vec<String>,

    /// Scripts to run after run completes.
    pub post_run: Vec<String>,

    /// Scripts to run before clean.
    pub pre_clean: Vec<String>,

    /// Scripts to run after clean.
    pub post_clean: Vec<String>,

    /// Scripts to run before lock generation.
    pub pre_lock: Vec<String>,

    /// Scripts to run after lock completes.
    pub post_lock: Vec<String>,

    /// Scripts to run on project initialization.
    pub init: Vec<String>,
}

/// Plugin configuration section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginConfig {
    /// Whether plugins are enabled.
    #[serde(default = "default_true")]
    pub enabled: bool,

    /// Timeout for hook execution in milliseconds.
    #[serde(default = "default_hook_timeout")]
    pub hook_timeout_ms: u64,

    /// Additional paths to search for plugins.
    #[serde(default)]
    pub paths: Vec<String>,

    /// Whether to continue on hook failure.
    #[serde(default)]
    pub continue_on_error: bool,

    /// Hook configuration.
    #[serde(default)]
    pub hooks: PluginHookConfig,
}

fn default_hook_timeout() -> u64 {
    5000
}

impl Default for PluginConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            hook_timeout_ms: default_hook_timeout(),
            paths: Vec::new(),
            continue_on_error: false,
            hooks: PluginHookConfig::default(),
        }
    }
}

/// Build configuration section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    /// GHC optimization level (0, 1, or 2)
    #[serde(default = "default_optimization")]
    pub optimization: u8,

    /// Enable all warnings (-Wall)
    #[serde(default = "default_true")]
    pub warnings: bool,

    /// Treat warnings as errors (-Werror)
    #[serde(default)]
    pub werror: bool,

    /// Additional GHC flags
    #[serde(default)]
    pub ghc_flags: Vec<String>,

    /// Source directories (defaults to ["src"])
    #[serde(default = "default_src_dirs")]
    pub src_dirs: Vec<String>,

    /// Use native build (experimental)
    #[serde(default)]
    pub native: bool,
}

fn default_optimization() -> u8 {
    1
}

fn default_src_dirs() -> Vec<String> {
    vec!["src".to_string()]
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            optimization: default_optimization(),
            warnings: true,
            werror: false,
            ghc_flags: Vec::new(),
            src_dirs: default_src_dirs(),
            native: false,
        }
    }
}

/// The hx.toml manifest.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Manifest {
    /// Project configuration
    pub project: ProjectConfig,

    /// Toolchain configuration
    #[serde(default)]
    pub toolchain: ToolchainConfig,

    /// Build configuration
    #[serde(default)]
    pub build: BuildConfig,

    /// Format configuration
    #[serde(default)]
    pub format: FormatConfig,

    /// Lint configuration
    #[serde(default)]
    pub lint: LintConfig,

    /// Plugin configuration
    #[serde(default)]
    pub plugins: PluginConfig,

    /// Dependencies (optional, can rely on .cabal)
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

impl Manifest {
    /// Parse a manifest from a TOML string.
    pub fn parse(s: &str) -> Result<Self, ManifestError> {
        Ok(toml::from_str(s)?)
    }

    /// Parse a manifest from a file.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, ManifestError> {
        let content = std::fs::read_to_string(path)?;
        Self::parse(&content)
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
            build: BuildConfig::default(),
            format: FormatConfig::default(),
            lint: LintConfig::default(),
            plugins: PluginConfig::default(),
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
        let manifest = Manifest::parse(toml).unwrap();
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
        let manifest = Manifest::parse(toml).unwrap();
        assert_eq!(manifest.project.name, "myapp");
        assert_eq!(manifest.toolchain.ghc, Some("9.8.2".to_string()));
        assert_eq!(manifest.format.formatter, "fourmolu");
        assert!(manifest.lint.hlint);
    }

    #[test]
    fn test_roundtrip() {
        let manifest = Manifest::new("test", ProjectKind::Lib);
        let toml = manifest.to_string().unwrap();
        let parsed = Manifest::parse(&toml).unwrap();
        assert_eq!(parsed.project.name, "test");
        assert_eq!(parsed.project.kind, ProjectKind::Lib);
    }
}
