//! Cabal wrapper and output parsing for hx.
//!
//! This crate handles:
//! - Invoking Cabal commands
//! - Parsing build output for errors
//! - Managing build directories

pub mod build;
pub mod freeze;

pub use build::{BuildOptions, BuildResult};
