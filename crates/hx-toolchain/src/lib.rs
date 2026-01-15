//! Toolchain detection and management for hx.
//!
//! This crate handles:
//! - Detecting GHC, Cabal, GHCup, and HLS
//! - Parsing tool versions
//! - Installing toolchains via ghcup

pub mod detect;
pub mod install;

pub use detect::{DetectedTool, ToolStatus, Toolchain};
