//! Configuration parsing and project detection for hx.
//!
//! This crate handles:
//! - Parsing `hx.toml` project manifests
//! - Detecting project roots
//! - Locating `.cabal` files
//! - Merging configuration defaults

pub mod combine;
pub mod manifest;
pub mod project;

pub use combine::Combine;
pub use manifest::{
    FormatConfig, LintConfig, Manifest, ProjectConfig, ProjectKind, ToolchainConfig,
};
pub use project::{find_project_root, Project};

/// The manifest filename.
pub const MANIFEST_FILENAME: &str = "hx.toml";

/// The lockfile filename.
pub const LOCKFILE_FILENAME: &str = "hx.lock";

/// The local cache directory name.
pub const CACHE_DIR_NAME: &str = ".hx";
