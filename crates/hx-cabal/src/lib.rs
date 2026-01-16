//! Cabal wrapper and output parsing for hx.
//!
//! This crate handles:
//! - Invoking Cabal commands
//! - Parsing build output for errors
//! - Managing build directories
//! - Editing .cabal files
//! - Parallel package fetching

pub mod build;
pub mod edit;
pub mod fetch;
pub mod freeze;

pub use build::{BuildOptions, BuildResult};
pub use edit::{CabalEditError, add_dependency, remove_dependency};
pub use fetch::{FetchOptions, FetchResult};
