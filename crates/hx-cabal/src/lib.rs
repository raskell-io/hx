//! Cabal wrapper and output parsing for hx.
//!
//! This crate handles:
//! - Invoking Cabal commands
//! - Parsing build output for errors
//! - Managing build directories
//! - Editing .cabal files
//! - Parallel package fetching
//! - Native GHC build orchestration

pub mod build;
pub mod edit;
pub mod fetch;
pub mod freeze;
pub mod native;

pub use build::{BuildOptions, BuildResult};
pub use edit::{CabalEditError, add_dependency, remove_dependency};
pub use fetch::{FetchOptions, FetchResult};
pub use native::{
    BuildState, GhcConfig, ModuleCompileResult, ModuleState, NativeBuildOptions,
    NativeBuildResult, NativeBuilder, compute_flags_hash, packages_from_lockfile,
};
