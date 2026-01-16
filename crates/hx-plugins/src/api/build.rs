//! Build context API for plugins.
//!
//! Provides: (hx/build-success?), (hx/build-duration), (hx/build-warnings), etc.
//! These are only available in post-build and post-test hooks.

use crate::context::with_context;
use crate::error::Result;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::SteelVal;

/// Register build context API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/build-success?", build_success);
    engine.register_fn("hx/build-duration", build_duration);
    engine.register_fn("hx/build-warnings", build_warnings);
    engine.register_fn("hx/build-errors", build_errors);
    engine.register_fn("hx/test-passed?", test_passed);
    engine.register_fn("hx/test-count", test_count);
    Ok(())
}

/// Check if the build succeeded.
fn build_success() -> SteelVal {
    with_context(|ctx| {
        ctx.build
            .as_ref()
            .map(|b| SteelVal::BoolV(b.success))
            .unwrap_or(SteelVal::BoolV(false))
    })
    .unwrap_or(SteelVal::BoolV(false))
}

/// Get the build duration in seconds.
fn build_duration() -> SteelVal {
    with_context(|ctx| {
        ctx.build
            .as_ref()
            .map(|b| SteelVal::NumV(b.duration.as_secs_f64()))
            .unwrap_or(SteelVal::NumV(0.0))
    })
    .unwrap_or(SteelVal::NumV(0.0))
}

/// Get the list of build warnings.
fn build_warnings() -> SteelVal {
    with_context(|ctx| {
        ctx.build
            .as_ref()
            .map(|b| {
                let warnings: Vec<SteelVal> = b
                    .warnings
                    .iter()
                    .map(|w| SteelVal::StringV(w.clone().into()))
                    .collect();
                SteelVal::ListV(warnings.into())
            })
            .unwrap_or_else(|| SteelVal::ListV(vec![].into()))
    })
    .unwrap_or_else(|| SteelVal::ListV(vec![].into()))
}

/// Get the list of build errors.
fn build_errors() -> SteelVal {
    with_context(|ctx| {
        ctx.build
            .as_ref()
            .map(|b| {
                let errors: Vec<SteelVal> = b
                    .errors
                    .iter()
                    .map(|e| SteelVal::StringV(e.clone().into()))
                    .collect();
                SteelVal::ListV(errors.into())
            })
            .unwrap_or_else(|| SteelVal::ListV(vec![].into()))
    })
    .unwrap_or_else(|| SteelVal::ListV(vec![].into()))
}

/// Check if all tests passed.
fn test_passed() -> SteelVal {
    with_context(|ctx| {
        ctx.test
            .as_ref()
            .map(|t| SteelVal::BoolV(t.passed))
            .unwrap_or(SteelVal::BoolV(false))
    })
    .unwrap_or(SteelVal::BoolV(false))
}

/// Get test counts as an association list: ((passed . N) (failed . N) (skipped . N)).
fn test_count() -> SteelVal {
    with_context(|ctx| {
        ctx.test.as_ref().map(|t| {
            SteelVal::ListV(
                vec![
                    SteelVal::ListV(
                        vec![
                            SteelVal::SymbolV("passed".into()),
                            SteelVal::IntV(t.passed_count as isize),
                        ]
                        .into(),
                    ),
                    SteelVal::ListV(
                        vec![
                            SteelVal::SymbolV("failed".into()),
                            SteelVal::IntV(t.failed_count as isize),
                        ]
                        .into(),
                    ),
                    SteelVal::ListV(
                        vec![
                            SteelVal::SymbolV("skipped".into()),
                            SteelVal::IntV(t.skipped_count as isize),
                        ]
                        .into(),
                    ),
                ]
                .into(),
            )
        })
    })
    .flatten()
    .unwrap_or_else(|| {
        SteelVal::ListV(
            vec![
                SteelVal::ListV(vec![SteelVal::SymbolV("passed".into()), SteelVal::IntV(0)].into()),
                SteelVal::ListV(vec![SteelVal::SymbolV("failed".into()), SteelVal::IntV(0)].into()),
                SteelVal::ListV(vec![SteelVal::SymbolV("skipped".into()), SteelVal::IntV(0)].into()),
            ]
            .into(),
        )
    })
}
