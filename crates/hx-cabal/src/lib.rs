//! Cabal wrapper and output parsing for hx.
//!
//! This crate handles:
//! - Invoking Cabal commands
//! - Parsing build output for errors
//! - Managing build directories
//! - Editing .cabal files
//! - Parallel package fetching
//! - Native GHC build orchestration
//! - GHC diagnostic parsing for LSP

pub mod build;
pub mod edit;
pub mod fetch;
pub mod freeze;
pub mod full_native;
pub mod ghc_diagnostics;
pub mod native;
pub mod package_build;
pub mod package_db;

pub use build::{BuildOptions, BuildResult};
pub use edit::{CabalEditError, add_dependency, remove_dependency};
pub use fetch::{FetchOptions, FetchResult};
pub use native::{
    BuildState, GhcConfig, ModuleCompileResult, ModuleState, NativeBuildOptions, NativeBuildResult,
    NativeBuilder, PackageInfo, collect_extra_libraries, collect_hs_libraries,
    collect_library_paths, compute_flags_hash, detect_main_module, find_all_main_modules,
    packages_from_lockfile, query_all_packages, query_package_info, resolve_transitive_deps,
};
pub use package_build::{PackageBuildConfig, PackageBuildResult, build_package};
pub use package_db::{PackageDb, clear_package_db, default_cache_dir};
pub use full_native::{
    FullNativeBuildOptions, FullNativeBuildResult, FullNativeBuilder, is_pre_installed,
    pre_installed_packages,
};
pub use ghc_diagnostics::{
    diagnostic_flags, parse_ghc_json, parse_ghc_text, supports_json_diagnostics,
};
