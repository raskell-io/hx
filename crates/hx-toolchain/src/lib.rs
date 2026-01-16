//! Toolchain detection and management for hx.
//!
//! This crate handles:
//! - Detecting GHC, Cabal, GHCup, and HLS
//! - Parsing tool versions
//! - Installing toolchains via ghcup
//! - Checking toolchain requirements and auto-installing
//! - Direct GHC binary downloads (replacing ghcup dependency)
//! - Per-project GHC version management

pub mod check;
pub mod detect;
pub mod ghc;
pub mod install;

pub use check::{AutoInstallPolicy, ToolchainCheck, check_requirements, ensure_toolchain};
pub use detect::{DetectedTool, ToolStatus, Toolchain};
pub use ghc::{
    DownloadOptions, GhcSource, GhcVersion, InstallResult, InstallSource, InstalledGhc, Platform,
    RECOMMENDED_GHC_VERSION, ResolutionConfig, ResolvedGhc, ToolchainManifest, create_symlinks,
    download_and_install_ghc, get_active, get_path_with_ghc, ghc_download_url, is_known_version,
    is_valid_version, known_versions, list_installed, remove_ghc, remove_symlinks, resolve_ghc,
    set_active,
};
pub use install::{InstallStrategy, SmartInstallOptions, install_ghc_smart};
