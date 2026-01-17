//! Command implementations.

mod audit;
mod bench;
mod build;
mod cache;
mod changelog;
mod clean;
mod completions;
mod coverage;
mod deps;
mod dist;
mod docs;
mod doctor;
mod fetch;
mod fmt;
mod ide;
mod import;
mod index;
mod init;
mod lint;
mod lock;
mod lsp;
mod new;
mod nix;
mod plugins;
mod profile;
mod publish;
mod run;
mod script;
mod search;
mod toolchain;
mod upgrade;
mod watch;

use crate::cli::{
    ArtifactCommands, CacheCommands, Cli, Commands, DepsCommands, DistCommands, GlobalArgs,
    GraphFormat, IndexCommands, NixCommands, PluginsCommands,
};
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
        Some(Commands::Add {
            package,
            constraint,
            dev,
        }) => deps::add(&package, constraint.as_deref(), dev, &output).await,
        Some(Commands::Rm { package }) => deps::remove(&package, &output).await,
        Some(Commands::Why { package }) => deps::why(&package, &output).await,
        Some(Commands::Outdated { direct, all }) => deps::outdated(direct, all, &output).await,
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
        Some(Commands::Docs {
            open,
            deps,
            serve,
            port,
        }) => docs::run(open, deps, serve, port, &output).await,
        Some(Commands::Changelog {
            unreleased,
            output: output_file,
            all,
            preview,
        }) => changelog::run(unreleased, output_file, all, preview, &output).await,
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
        Some(Commands::Nix { command }) => match command {
            NixCommands::Flake {
                output: out_file,
                force,
            } => nix::flake(out_file, force, &output).await,
            NixCommands::Shell {
                output: out_file,
                force,
            } => nix::shell(out_file, force, &output).await,
        },
        Some(Commands::Profile {
            heap,
            time,
            detail,
            args,
        }) => profile::run(heap, time, detail, args, policy, &output).await,
        Some(Commands::Script { file, args }) => script::run(&file, args, &output).await,
        Some(Commands::Import { from, path }) => import::run(from, path, &output).await,
        Some(Commands::Search {
            query,
            limit,
            detailed,
        }) => search::run(&query, limit, detailed, &output).await,
        Some(Commands::Audit {
            fix,
            ignore,
            outdated,
            licenses,
        }) => audit::run(fix, ignore, outdated, licenses, &output).await,
        Some(Commands::Watch {
            test,
            clear,
            debounce,
            package,
        }) => watch::run(test, clear, Some(debounce), package, policy, &output).await,
        Some(Commands::Deps { command }) => match command {
            DepsCommands::Graph {
                format,
                depth,
                highlight,
                output: output_file,
                dev,
                direct,
            } => deps::graph(format, depth, highlight, output_file, dev, direct, &output).await,
            DepsCommands::Tree { depth, dev } => {
                deps::graph(GraphFormat::Tree, depth, None, None, dev, false, &output).await
            }
            DepsCommands::List { dev, direct } => {
                deps::graph(GraphFormat::List, 0, None, None, dev, direct, &output).await
            }
        },
        Some(Commands::Lsp { tcp }) => lsp::run(tcp, &output).await,
        Some(Commands::Plugins { command }) => match command {
            PluginsCommands::List => plugins::list(&output).await,
            PluginsCommands::Status => plugins::status(&output).await,
            PluginsCommands::Run { script, args } => {
                plugins::run_script(script, args, &output).await
            }
        },
        Some(Commands::Coverage {
            html,
            open,
            output: output_dir,
            threshold,
            package,
            pattern,
            json,
            exclude,
        }) => {
            let config = coverage::CoverageConfig {
                html,
                open,
                output_dir,
                threshold,
                package,
                pattern,
                json,
                exclude,
            };
            coverage::run(config, policy, &output).await
        }
        Some(Commands::Dist {
            command,
            target,
            output: output_dir,
            strip,
            completions,
            version,
        }) => match command {
            Some(DistCommands::Formula {
                version,
                output: out_file,
            }) => dist::generate_formula(version, out_file, &output),
            Some(DistCommands::InstallScript {
                version,
                output: out_file,
            }) => dist::generate_install_script(version, out_file, &output),
            None => {
                let config = dist::DistConfig {
                    target,
                    output_dir,
                    strip,
                    include_completions: completions,
                    version,
                };
                dist::run(config, &output).await
            }
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
