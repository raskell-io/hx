//! Shell command execution API for plugins.
//!
//! Provides: (hx/run), (hx/run-checked), (hx/run-silent)

use crate::context::with_context;
use crate::error::Result;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::SteelVal;
use std::process::Command;

/// Register shell API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/run", run_command);
    engine.register_fn("hx/run-checked", run_checked);
    engine.register_fn("hx/run-silent", run_silent);
    Ok(())
}

/// Run a command and return an association list with exit-code, stdout, stderr.
/// Returns: ((exit-code . N) (stdout . "...") (stderr . "..."))
fn run_command(cmd: String, args: Vec<SteelVal>) -> SteelVal {
    let args: Vec<String> = args
        .into_iter()
        .filter_map(|v| match v {
            SteelVal::StringV(s) => Some(s.to_string()),
            _ => None,
        })
        .collect();

    // Get environment variables from context
    let env_vars = with_context(|ctx| ctx.env_vars.clone()).unwrap_or_default();

    let result = Command::new(&cmd)
        .args(&args)
        .envs(&env_vars)
        .output();

    match result {
        Ok(output) => {
            // Return as association list: ((exit-code . N) (stdout . "...") (stderr . "..."))
            let exit_code = SteelVal::IntV(output.status.code().unwrap_or(-1) as isize);
            let stdout = SteelVal::StringV(String::from_utf8_lossy(&output.stdout).to_string().into());
            let stderr = SteelVal::StringV(String::from_utf8_lossy(&output.stderr).to_string().into());

            SteelVal::ListV(
                vec![
                    SteelVal::ListV(vec![SteelVal::SymbolV("exit-code".into()), exit_code].into()),
                    SteelVal::ListV(vec![SteelVal::SymbolV("stdout".into()), stdout].into()),
                    SteelVal::ListV(vec![SteelVal::SymbolV("stderr".into()), stderr].into()),
                ]
                .into(),
            )
        }
        Err(e) => {
            SteelVal::ListV(
                vec![
                    SteelVal::ListV(vec![SteelVal::SymbolV("exit-code".into()), SteelVal::IntV(-1)].into()),
                    SteelVal::ListV(vec![SteelVal::SymbolV("stdout".into()), SteelVal::StringV("".into())].into()),
                    SteelVal::ListV(vec![SteelVal::SymbolV("stderr".into()), SteelVal::StringV(e.to_string().into())].into()),
                ]
                .into(),
            )
        }
    }
}

/// Run a command and return stdout, or raise an error on failure.
fn run_checked(cmd: String, args: Vec<SteelVal>) -> std::result::Result<SteelVal, String> {
    let args: Vec<String> = args
        .into_iter()
        .filter_map(|v| match v {
            SteelVal::StringV(s) => Some(s.to_string()),
            _ => None,
        })
        .collect();

    let env_vars = with_context(|ctx| ctx.env_vars.clone()).unwrap_or_default();

    let output = Command::new(&cmd)
        .args(&args)
        .envs(&env_vars)
        .output()
        .map_err(|e| format!("Failed to execute {}: {}", cmd, e))?;

    if output.status.success() {
        Ok(SteelVal::StringV(
            String::from_utf8_lossy(&output.stdout).to_string().into(),
        ))
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!(
            "Command '{}' failed with exit code {:?}: {}",
            cmd,
            output.status.code(),
            stderr
        ))
    }
}

/// Run a command silently and return just the exit code.
fn run_silent(cmd: String, args: Vec<SteelVal>) -> SteelVal {
    let args: Vec<String> = args
        .into_iter()
        .filter_map(|v| match v {
            SteelVal::StringV(s) => Some(s.to_string()),
            _ => None,
        })
        .collect();

    let env_vars = with_context(|ctx| ctx.env_vars.clone()).unwrap_or_default();

    let result = Command::new(&cmd)
        .args(&args)
        .envs(&env_vars)
        .output();

    match result {
        Ok(output) => SteelVal::IntV(output.status.code().unwrap_or(-1) as isize),
        Err(_) => SteelVal::IntV(-1),
    }
}
