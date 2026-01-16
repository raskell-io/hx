//! Steel API functions for hx plugins.
//!
//! All functions are registered under the `hx/` namespace.

pub mod build;
pub mod config;
pub mod fs;
pub mod output;
pub mod project;
pub mod shell;

use crate::error::Result;
use steel::steel_vm::engine::Engine;

/// Register all hx API functions with the Steel engine.
pub fn register_all(engine: &mut Engine) -> Result<()> {
    project::register(engine)?;
    shell::register(engine)?;
    config::register(engine)?;
    output::register(engine)?;
    fs::register(engine)?;
    build::register(engine)?;
    Ok(())
}
