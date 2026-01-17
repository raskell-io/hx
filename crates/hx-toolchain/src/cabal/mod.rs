//! Cabal version manager.
//!
//! Direct Cabal binary downloads and version management without ghcup dependency.
//! This module provides:
//! - Platform detection for download URLs
//! - Known Cabal versions and validation
//! - Installation tracking via manifest
//! - Download and extraction

pub mod download;
pub mod platform;
pub mod versions;

pub use download::{
    CabalDownloadOptions, CabalInstallResult, download_and_install_cabal, get_active_cabal,
    list_installed, remove_cabal, set_active_cabal,
};
pub use platform::{cabal_archive_filename, cabal_download_url, cabal_platform_suffix};
pub use versions::{
    CabalVersion, KNOWN_CABAL_VERSIONS, RECOMMENDED_CABAL_VERSION, is_known_version,
    is_valid_version, known_versions,
};
