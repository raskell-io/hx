//! Configuration API for plugins.
//!
//! Provides: (hx/config-get), (hx/env-get), (hx/env-set!)

use crate::context::{with_context, with_context_mut};
use crate::error::Result;
use std::env;
use steel::SteelVal;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;

/// Register config API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/config-get", config_get);
    engine.register_fn("hx/env-get", env_get);
    engine.register_fn("hx/env-set!", env_set);
    engine.register_fn("hx/verbose?", is_verbose);
    Ok(())
}

/// Get a configuration value.
/// Note: This is a placeholder - full implementation would need access to hx.toml.
fn config_get(_section: String, _key: String) -> SteelVal {
    // TODO: Implement config access once we have the manifest in context
    SteelVal::BoolV(false)
}

/// Get an environment variable.
fn env_get(name: String) -> SteelVal {
    match env::var(&name) {
        Ok(value) => SteelVal::StringV(value.into()),
        Err(_) => SteelVal::BoolV(false),
    }
}

/// Set an environment variable for child processes.
fn env_set(name: String, value: String) -> SteelVal {
    with_context_mut(|ctx| {
        ctx.set_env(name, value);
    });
    SteelVal::Void
}

/// Check if verbose mode is enabled.
fn is_verbose() -> SteelVal {
    with_context(|ctx| SteelVal::BoolV(ctx.verbose)).unwrap_or(SteelVal::BoolV(false))
}
