//! Command implementations.

mod bench;
mod build;
mod cache;
mod clean;
mod completions;
mod deps;
mod doctor;
mod fetch;
mod fmt;
mod ide;
mod index;
mod init;
mod lint;
mod lock;
mod new;
mod publish;
mod run;
mod toolchain;
mod upgrade;

use crate::cli::{ArtifactCommands, CacheCommands, Cli, Commands, GlobalArgs, IndexCommands};
use anyhow::Result;
use hx_toolchain::AutoInstallPolicy;
use hx_ui::{Output, Printer, Verbosity};

/// Determine auto-install policy from global args.
fn auto_install_policy(global: &GlobalArgs) -> AutoInstallPolicy {
    if global.no_auto_install {
        AutoInstallPolicy::Never
    } else if global.auto_install {
        AutoInstallPolicy::Always
    } else {
        AutoInstallPolicy::Prompt
    }
}

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

    let policy = auto_install_policy(&cli.global);

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
            package,
            native,
        }) => build::run(release, jobs, target, package, native, policy, &output).await,
        Some(Commands::Test { pattern, package }) => {
            build::test(pattern, package, policy, &output).await
        }
        Some(Commands::Run { args, package }) => run::run(args, package, policy, &output).await,
        Some(Commands::Repl) => run::repl(policy, &output).await,
        Some(Commands::Check) => {
            // Check is just a fast build
            build::run(false, None, None, None, false, policy, &output).await
        }
        Some(Commands::Fmt { check }) => fmt::run(check, &output).await,
        Some(Commands::Lint { fix }) => lint::run(fix, &output).await,
        Some(Commands::Doctor) => doctor::run(&output).await,
        Some(Commands::Lock { cabal, update }) => lock::run(cabal, update, &output).await,
        Some(Commands::Fetch { jobs }) => fetch::run(jobs, &output).await,
        Some(Commands::Sync { force }) => lock::sync(force, &output).await,
        Some(Commands::Clean { global }) => clean::run(global, &output).await,
        Some(Commands::Toolchain { command }) => toolchain::run(command, &output).await,
        Some(Commands::Add { package, dev }) => deps::add(&package, dev, &output).await,
        Some(Commands::Rm { package }) => deps::remove(&package, &output).await,
        Some(Commands::Completions { shell }) => completions::run(shell),
        Some(Commands::Upgrade {
            check,
            target_version,
        }) => upgrade::run(check, target_version, &output).await,
        Some(Commands::New { command }) => new::run(command, &output).await,
        Some(Commands::Bench {
            filter,
            save_baseline,
            baseline,
            package,
        }) => bench::run(filter, save_baseline, baseline, package, policy, &output).await,
        Some(Commands::Publish {
            dry_run,
            username,
            password,
            docs,
        }) => publish::run(dry_run, username, password, docs, &output).await,
        Some(Commands::Ide { command }) => ide::run(command, &output).await,
        Some(Commands::Index { command }) => match command {
            IndexCommands::Update { force, staleness } => {
                index::update(force, staleness, &output).await
            }
            IndexCommands::Status => index::status(&output).await,
            IndexCommands::Clear { yes } => index::clear(yes, &output).await,
        },
        Some(Commands::Cache { command }) => match command {
            CacheCommands::Status => cache::status(&output).await,
            CacheCommands::Prune { days } => cache::prune(days, &output).await,
            CacheCommands::Clean => cache::clean(&output).await,
            CacheCommands::Artifacts { command } => match command {
                ArtifactCommands::Status => cache::artifacts_status(&output).await,
                ArtifactCommands::Prune { days } => cache::artifacts_prune(days, &output).await,
                ArtifactCommands::Clear => cache::artifacts_clear(&output).await,
            },
        },
        None => {
            // No command - show help
            use clap::CommandFactory;
            Cli::command().print_help()?;
            println!();
            Ok(0)
        }
    }
}
