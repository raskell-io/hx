//! Command implementations.

mod build;
mod clean;
mod deps;
mod doctor;
mod fmt;
mod init;
mod lint;
mod lock;
mod run;
mod toolchain;

use crate::cli::{Cli, Commands};
use anyhow::Result;
use hx_ui::{Output, Printer, Verbosity};

/// Run the CLI command.
pub async fn run(cli: Cli) -> Result<i32> {
    // Create printer from global args
    let printer = Printer::from_flags(cli.global.quiet, cli.global.verbose);

    // Create output handler (for backwards compatibility)
    let output = Output::with_verbosity(if printer.is_verbose() {
        Verbosity::Verbose
    } else if printer.is_quiet() {
        Verbosity::Quiet
    } else {
        Verbosity::Normal
    });

    match cli.command {
        Some(Commands::Init {
            bin,
            lib,
            name,
            dir,
            ci,
        }) => init::run(bin, lib, name, dir, ci, &output).await,
        Some(Commands::Build {
            release,
            jobs,
            target,
        }) => build::run(release, jobs, target, &output).await,
        Some(Commands::Test { pattern }) => build::test(pattern, &output).await,
        Some(Commands::Run { args }) => run::run(args, &output).await,
        Some(Commands::Repl) => run::repl(&output).await,
        Some(Commands::Check) => {
            // Check is just a fast build
            build::run(false, None, None, &output).await
        }
        Some(Commands::Fmt { check }) => fmt::run(check, &output).await,
        Some(Commands::Lint { fix }) => lint::run(fix, &output).await,
        Some(Commands::Doctor) => doctor::run(&output).await,
        Some(Commands::Lock) => lock::run(&output).await,
        Some(Commands::Sync { force }) => lock::sync(force, &output).await,
        Some(Commands::Clean { global }) => clean::run(global, &output).await,
        Some(Commands::Toolchain { command }) => toolchain::run(command, &output).await,
        Some(Commands::Add { package, dev }) => deps::add(&package, dev, &output).await,
        Some(Commands::Rm { package }) => deps::remove(&package, &output).await,
        None => {
            // No command - show help
            use clap::CommandFactory;
            Cli::command().print_help()?;
            println!();
            Ok(0)
        }
    }
}
