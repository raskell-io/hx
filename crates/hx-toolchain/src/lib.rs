//! Toolchain detection and management for hx.
//!
//! This crate handles:
//! - Detecting GHC, Cabal, GHCup, and HLS
//! - Parsing tool versions
//! - Installing toolchains via ghcup
//! - Checking toolchain requirements and auto-installing

pub mod check;
pub mod detect;
pub mod install;

pub use check::{AutoInstallPolicy, ToolchainCheck, check_requirements, ensure_toolchain};
pub use detect::{DetectedTool, ToolStatus, Toolchain};
