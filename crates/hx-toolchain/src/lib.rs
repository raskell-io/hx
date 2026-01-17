//! Toolchain detection and management for hx.
//!
//! This crate handles:
//! - Detecting GHC, Cabal, GHCup, and HLS
//! - Parsing tool versions
//! - Installing toolchains via ghcup or direct download
//! - Checking toolchain requirements and auto-installing
//! - Direct GHC and Cabal binary downloads (no ghcup dependency required)
//! - Per-project toolchain version management

pub mod cabal;
pub mod check;
pub mod detect;
pub mod ghc;
pub mod install;

pub use cabal::{
    CabalDownloadOptions, CabalInstallResult, CabalVersion, KNOWN_CABAL_VERSIONS,
    RECOMMENDED_CABAL_VERSION, cabal_download_url, download_and_install_cabal, get_active_cabal,
    is_known_version as is_known_cabal_version, is_valid_version as is_valid_cabal_version,
    known_versions as known_cabal_versions, list_installed as list_installed_cabal, remove_cabal,
    set_active_cabal,
};
pub use check::{AutoInstallPolicy, ToolchainCheck, check_requirements, ensure_toolchain};
pub use detect::{DetectedTool, ToolStatus, Toolchain};
pub use ghc::{
    DownloadOptions, GhcSource, GhcVersion, InstallResult, InstallSource, InstalledCabal,
    InstalledGhc, Platform, RECOMMENDED_GHC_VERSION, ResolutionConfig, ResolvedGhc,
    ToolchainManifest, create_symlinks, download_and_install_ghc, get_active, get_path_with_ghc,
    ghc_download_url, is_known_version, is_valid_version, known_versions, list_installed,
    remove_ghc, remove_symlinks, resolve_ghc, set_active,
};
pub use install::{
    InstallStrategy, SmartCabalInstallOptions, SmartInstallOptions, install_cabal_smart,
    install_ghc_smart,
};
