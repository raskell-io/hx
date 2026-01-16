//! CLI argument parsing.

use clap::{Args, Parser, Subcommand};
use clap_complete::Shell;
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

    /// Disable automatic toolchain installation
    #[arg(long, global = true, env = "HX_NO_AUTO_INSTALL")]
    pub no_auto_install: bool,

    /// Always install missing toolchain components without prompting
    #[arg(long, global = true, env = "HX_AUTO_INSTALL")]
    pub auto_install: bool,
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

        /// Build specific package (in workspace)
        #[arg(short, long)]
        package: Option<String>,
    },

    /// Run tests
    Test {
        /// Test pattern to match
        #[arg(short, long)]
        pattern: Option<String>,

        /// Test specific package (in workspace)
        #[arg(long)]
        package: Option<String>,
    },

    /// Run the project
    Run {
        /// Arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,

        /// Run specific package (in workspace)
        #[arg(short, long)]
        package: Option<String>,
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
    Lock {
        /// Use cabal freeze instead of native solver
        #[arg(long)]
        cabal: bool,

        /// Update specific packages (or all if none specified)
        #[arg(long, num_args = 0..)]
        update: Option<Vec<String>>,
    },

    /// Pre-fetch dependencies in parallel
    Fetch {
        /// Number of parallel download jobs
        #[arg(short, long)]
        jobs: Option<usize>,
    },

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

    /// Manage the build cache
    Cache {
        #[command(subcommand)]
        command: CacheCommands,
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

    /// Generate shell completions
    Completions {
        /// Shell to generate completions for
        #[arg(value_enum)]
        shell: Shell,
    },

    /// Upgrade hx to the latest version
    Upgrade {
        /// Check for updates without installing
        #[arg(long)]
        check: bool,

        /// Upgrade to a specific version
        #[arg(long = "target")]
        target_version: Option<String>,
    },

    /// Create a new module or file from a template
    New {
        #[command(subcommand)]
        command: NewCommands,
    },

    /// Run benchmarks
    Bench {
        /// Benchmark filter pattern
        #[arg(short, long)]
        filter: Option<String>,

        /// Save baseline for comparison
        #[arg(long)]
        save_baseline: Option<String>,

        /// Compare against baseline
        #[arg(long)]
        baseline: Option<String>,

        /// Benchmark specific package (in workspace)
        #[arg(short, long)]
        package: Option<String>,
    },

    /// Publish package to Hackage
    Publish {
        /// Perform a dry run without uploading
        #[arg(long)]
        dry_run: bool,

        /// Hackage username
        #[arg(long, env = "HACKAGE_USERNAME")]
        username: Option<String>,

        /// Hackage password
        #[arg(long, env = "HACKAGE_PASSWORD")]
        password: Option<String>,

        /// Publish documentation
        #[arg(long)]
        docs: bool,
    },

    /// Configure IDE integration (HLS, hie.yaml)
    Ide {
        #[command(subcommand)]
        command: IdeCommands,
    },
}

#[derive(Subcommand, Debug)]
pub enum IdeCommands {
    /// Generate or update hie.yaml for HLS
    Setup {
        /// Overwrite existing hie.yaml
        #[arg(long)]
        force: bool,
    },

    /// Check IDE/HLS configuration status
    Status,
}

#[derive(Subcommand, Debug)]
pub enum CacheCommands {
    /// Show cache status and statistics
    Status,

    /// Prune old cache entries
    Prune {
        /// Maximum age in days (default: 30)
        #[arg(long, default_value = "30")]
        days: u64,
    },

    /// Clean the entire cache
    Clean,
}

#[derive(Subcommand, Debug)]
pub enum NewCommands {
    /// Create a new Haskell module
    Module {
        /// Module name (e.g., Data.List.Extra)
        name: String,

        /// Create in src directory (default)
        #[arg(long, conflicts_with = "test")]
        src: bool,

        /// Create in test directory
        #[arg(long)]
        test: bool,
    },

    /// Create a new test module
    Test {
        /// Test module name
        name: String,
    },

    /// Create a new benchmark module
    Benchmark {
        /// Benchmark module name
        name: String,
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
