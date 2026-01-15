//! CLI argument parsing.

use clap::{Args, Parser, Subcommand};
use hx_core::EnvVars;

use crate::styles::STYLES;

/// hx - Haskell Toolchain CLI
#[derive(Parser, Debug)]
#[command(name = "hx")]
#[command(author, version, about = "A fast, opinionated Haskell toolchain CLI")]
#[command(long_about = None)]
#[command(propagate_version = true)]
#[command(styles = STYLES)]
#[command(after_help = "Use `hx help <command>` for more information about a command.")]
pub struct Cli {
    #[command(flatten)]
    pub global: GlobalArgs,

    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Global arguments available to all commands.
#[derive(Args, Debug)]
pub struct GlobalArgs {
    /// Enable verbose output
    #[arg(short, long, global = true, env = EnvVars::HX_VERBOSE)]
    pub verbose: bool,

    /// Suppress output (use twice for complete silence)
    #[arg(short, long, global = true, action = clap::ArgAction::Count, env = EnvVars::HX_QUIET)]
    pub quiet: u8,

    /// Disable colored output
    #[arg(long, global = true, env = EnvVars::HX_NO_COLOR)]
    pub no_color: bool,

    /// Path to configuration file
    #[arg(long, global = true, env = EnvVars::HX_CONFIG_FILE)]
    pub config_file: Option<std::path::PathBuf>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Initialize a new Haskell project
    Init {
        /// Create a binary project
        #[arg(long, conflicts_with = "lib")]
        bin: bool,

        /// Create a library project
        #[arg(long)]
        lib: bool,

        /// Project name
        #[arg(long)]
        name: Option<String>,

        /// Target directory
        #[arg(long)]
        dir: Option<String>,

        /// Generate GitHub Actions CI workflow
        #[arg(long)]
        ci: bool,
    },

    /// Build the project
    Build {
        /// Build in release mode with optimizations
        #[arg(long)]
        release: bool,

        /// Number of parallel jobs
        #[arg(short, long)]
        jobs: Option<usize>,

        /// Target triple
        #[arg(long)]
        target: Option<String>,
    },

    /// Run tests
    Test {
        /// Test pattern to match
        #[arg(short, long)]
        pattern: Option<String>,
    },

    /// Run the project
    Run {
        /// Arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },

    /// Start a REPL
    Repl,

    /// Type-check the project (fast build)
    Check,

    /// Format source code
    Fmt {
        /// Check formatting without making changes
        #[arg(long)]
        check: bool,
    },

    /// Run linter
    Lint {
        /// Apply automatic fixes
        #[arg(long)]
        fix: bool,
    },

    /// Diagnose toolchain and project issues
    Doctor,

    /// Create or update the lockfile
    Lock,

    /// Build with locked dependencies
    Sync {
        /// Force sync even if lock is outdated
        #[arg(long)]
        force: bool,
    },

    /// Clean build artifacts
    Clean {
        /// Clean global cache instead of project cache
        #[arg(long)]
        global: bool,
    },

    /// Manage toolchain versions
    Toolchain {
        #[command(subcommand)]
        command: ToolchainCommands,
    },

    /// Add a dependency
    Add {
        /// Package name
        package: String,

        /// Add as a development dependency
        #[arg(long)]
        dev: bool,
    },

    /// Remove a dependency
    Rm {
        /// Package name
        package: String,
    },
}

#[derive(Subcommand, Debug)]
pub enum ToolchainCommands {
    /// Show installed toolchain versions
    Status,

    /// Install toolchain components
    Install {
        /// GHC version to install
        #[arg(long)]
        ghc: Option<String>,

        /// Cabal version to install
        #[arg(long)]
        cabal: Option<String>,

        /// HLS version to install
        #[arg(long)]
        hls: Option<String>,
    },

    /// Set the active toolchain version
    Use {
        /// Profile name or version
        profile: String,
    },
}
