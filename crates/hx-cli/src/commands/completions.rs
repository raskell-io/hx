//! Shell completion generation.

use anyhow::Result;
use clap::CommandFactory;
use clap_complete::{Shell, generate};

use crate::cli::Cli;

/// Generate shell completions and print to stdout.
pub fn run(shell: Shell) -> Result<i32> {
    let mut cmd = Cli::command();
    let name = cmd.get_name().to_string();

    generate(shell, &mut cmd, name, &mut std::io::stdout());

    Ok(0)
}
