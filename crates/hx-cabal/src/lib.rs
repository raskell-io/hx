//! Cabal wrapper and output parsing for hx.
//!
//! This crate handles:
//! - Invoking Cabal commands
//! - Parsing build output for errors
//! - Managing build directories
//! - Editing .cabal files

pub mod build;
pub mod edit;
pub mod freeze;

pub use build::{BuildOptions, BuildResult};
pub use edit::{add_dependency, remove_dependency, CabalEditError};
