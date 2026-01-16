//! Output API for plugins.
//!
//! Provides: (hx/status), (hx/info), (hx/warn), (hx/error), (hx/debug)

use crate::context::with_context;
use crate::error::Result;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::SteelVal;

/// Register output API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/status", status);
    engine.register_fn("hx/info", info);
    engine.register_fn("hx/warn", warn);
    engine.register_fn("hx/error", error);
    engine.register_fn("hx/debug", debug);
    Ok(())
}

/// Print a status message: [action] message
fn status(action: String, message: String) -> SteelVal {
    // Use ANSI colors for output
    eprintln!("\x1b[1;32m[{}]\x1b[0m {}", action, message);
    SteelVal::Void
}

/// Print an info message.
fn info(message: String) -> SteelVal {
    eprintln!("{}", message);
    SteelVal::Void
}

/// Print a warning message in yellow.
fn warn(message: String) -> SteelVal {
    eprintln!("\x1b[1;33mwarning:\x1b[0m {}", message);
    SteelVal::Void
}

/// Print an error message in red.
fn error(message: String) -> SteelVal {
    eprintln!("\x1b[1;31merror:\x1b[0m {}", message);
    SteelVal::Void
}

/// Print a debug message (only if verbose).
fn debug(message: String) -> SteelVal {
    let verbose = with_context(|ctx| ctx.verbose).unwrap_or(false);
    if verbose {
        eprintln!("\x1b[2mdebug:\x1b[0m {}", message);
    }
    SteelVal::Void
}
