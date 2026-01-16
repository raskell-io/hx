//! Configuration parsing and project detection for hx.
//!
//! This crate handles:
//! - Parsing `hx.toml` project manifests
//! - Detecting project roots
//! - Locating `.cabal` files
//! - Merging configuration defaults
//! - IDE/HLS configuration (hie.yaml generation)

pub mod combine;
pub mod ide;
pub mod manifest;
pub mod project;

pub use combine::Combine;
pub use ide::{
    HieYamlStatus, check_hie_yaml, generate_hie_yaml, is_hls_compatible, recommended_hls_for_ghc,
    write_hie_yaml,
};
pub use manifest::{
    BuildConfig, FormatConfig, LintConfig, Manifest, PluginConfig, PluginHookConfig, ProjectConfig,
    ProjectKind, ToolchainConfig,
};
pub use project::{Project, WorkspacePackage, find_project_root};

/// The manifest filename.
pub const MANIFEST_FILENAME: &str = "hx.toml";

/// The lockfile filename.
pub const LOCKFILE_FILENAME: &str = "hx.lock";

/// The local cache directory name.
pub const CACHE_DIR_NAME: &str = ".hx";
