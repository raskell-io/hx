//! Project information API for plugins.
//!
//! Provides: (hx/project-name), (hx/project-root), (hx/ghc-version), etc.

use crate::context::with_context;
use crate::error::Result;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::SteelVal;

/// Register project API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/project-name", project_name);
    engine.register_fn("hx/project-root", project_root);
    engine.register_fn("hx/ghc-version", ghc_version);
    engine.register_fn("hx/cabal-file", cabal_file);
    Ok(())
}

/// Get the project name.
fn project_name() -> SteelVal {
    with_context(|ctx| SteelVal::StringV(ctx.project_name.clone().into()))
        .unwrap_or(SteelVal::BoolV(false))
}

/// Get the project root directory.
fn project_root() -> SteelVal {
    with_context(|ctx| {
        SteelVal::StringV(ctx.project_root.to_string_lossy().to_string().into())
    })
    .unwrap_or(SteelVal::BoolV(false))
}

/// Get the GHC version.
fn ghc_version() -> SteelVal {
    with_context(|ctx| {
        ctx.ghc_version
            .as_ref()
            .map(|v| SteelVal::StringV(v.clone().into()))
            .unwrap_or(SteelVal::BoolV(false))
    })
    .unwrap_or(SteelVal::BoolV(false))
}

/// Get the cabal file path.
fn cabal_file() -> SteelVal {
    with_context(|ctx| {
        ctx.cabal_file
            .as_ref()
            .map(|p| SteelVal::StringV(p.to_string_lossy().to_string().into()))
            .unwrap_or(SteelVal::BoolV(false))
    })
    .unwrap_or(SteelVal::BoolV(false))
}
