//! hx - Haskell Toolchain CLI
//!
//! A fast, opinionated, batteries-included toolchain for Haskell.

use anyhow::Result;
use clap::Parser;

mod cli;
mod commands;
mod styles;

use cli::Cli;

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize telemetry based on verbosity
    hx_telemetry::init(cli.global.verbose);

    // Enable warnings if not quiet
    if cli.global.quiet == 0 {
        hx_warnings::enable();
    }

    // Run the command
    let exit_code = commands::run(cli).await?;

    std::process::exit(exit_code);
}
