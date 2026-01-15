//! hx - Haskell Toolchain CLI
//!
//! A fast, opinionated, batteries-included toolchain for Haskell.

use anyhow::Result;
use clap::Parser;

mod cli;
mod commands;

use cli::Cli;

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize telemetry
    hx_telemetry::init(cli.verbose);

    // Run the command
    let exit_code = commands::run(cli).await?;

    std::process::exit(exit_code);
}
