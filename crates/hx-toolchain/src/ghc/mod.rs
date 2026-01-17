//! GHC version manager.
//!
//! Direct GHC binary downloads and version management without ghcup dependency.
//! This module provides:
//! - Platform detection for download URLs
//! - Known GHC versions and validation
//! - Installation tracking via manifest
//! - Download and extraction
//! - Version activation and resolution

pub mod activate;
pub mod download;
pub mod manifest;
pub mod platform;
pub mod versions;

pub use activate::{
    GhcSource, ResolutionConfig, ResolvedGhc, create_symlinks, get_path_with_ghc, remove_symlinks,
    resolve_ghc,
};
pub use download::{
    DownloadOptions, InstallResult, download_and_install_ghc, get_active, list_installed,
    remove_ghc, set_active,
};
pub use manifest::{InstallSource, InstalledCabal, InstalledGhc, ToolchainManifest};
pub use platform::{Platform, ghc_archive_filename, ghc_download_url};
pub use versions::{
    GhcVersion, KNOWN_GHC_VERSIONS, RECOMMENDED_GHC_VERSION, is_known_version, is_valid_version,
    known_versions,
};
