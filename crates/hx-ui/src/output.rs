//! Output formatting for the hx CLI.

use crate::style::Style;
use hx_core::error::{Error, Fix};
use std::io::{self, Write};

/// Verbosity level for output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Verbosity {
    /// Quiet mode - minimal output
    Quiet,
    /// Normal output
    #[default]
    Normal,
    /// Verbose output - includes underlying tool output
    Verbose,
}

/// Output handler for consistent CLI output.
#[derive(Debug, Clone)]
pub struct Output {
    verbosity: Verbosity,
}

impl Default for Output {
    fn default() -> Self {
        Self::new()
    }
}

impl Output {
    /// Create a new output handler with default verbosity.
    pub fn new() -> Self {
        Self {
            verbosity: Verbosity::Normal,
        }
    }

    /// Create an output handler with specified verbosity.
    pub fn with_verbosity(verbosity: Verbosity) -> Self {
        Self { verbosity }
    }

    /// Get the current verbosity level.
    pub fn verbosity(&self) -> Verbosity {
        self.verbosity
    }

    /// Check if verbose output is enabled.
    pub fn is_verbose(&self) -> bool {
        self.verbosity >= Verbosity::Verbose
    }

    /// Print a status message with a step title.
    pub fn status(&self, action: &str, message: &str) {
        if self.verbosity >= Verbosity::Normal {
            eprintln!("{:>12} {}", Style::bold(Style::success(action)), message);
        }
    }

    /// Print an info message.
    pub fn info(&self, message: &str) {
        if self.verbosity >= Verbosity::Normal {
            eprintln!("{}", message);
        }
    }

    /// Print a warning message.
    pub fn warn(&self, message: &str) {
        eprintln!("{}: {}", Style::warning("warning"), message);
    }

    /// Print an error message.
    pub fn error(&self, message: &str) {
        eprintln!("{}: {}", Style::error("error"), message);
    }

    /// Print verbose output (only shown in verbose mode).
    pub fn verbose(&self, message: &str) {
        if self.verbosity >= Verbosity::Verbose {
            eprintln!("{}", Style::dim(message));
        }
    }

    /// Print a structured error with fixes.
    pub fn print_error(&self, error: &Error) {
        eprintln!();
        eprintln!("{}: {}", Style::error("error"), error);

        // Print context if available
        match error {
            Error::Config { path: Some(p), .. } => {
                eprintln!("  {} {}", Style::dim("-->"), p.display());
            }
            Error::Io { path: Some(p), .. } => {
                eprintln!("  {} {}", Style::dim("-->"), p.display());
            }
            Error::ToolchainMismatch {
                expected, found, ..
            } => {
                eprintln!("  {} {}", Style::dim("expected:"), expected);
                eprintln!("  {} {}", Style::dim("found:"), found);
            }
            _ => {}
        }

        // Print fixes
        let fixes = error.fixes();
        if !fixes.is_empty() {
            eprintln!();
            for fix in fixes {
                self.print_fix(fix);
            }
        }
    }

    /// Print a fix suggestion.
    pub fn print_fix(&self, fix: &Fix) {
        if let Some(ref cmd) = fix.command {
            eprintln!("{}: Run `{}`", Style::info("fix"), Style::command(cmd));
            if fix.description != *cmd {
                eprintln!("      {}", Style::dim(&fix.description));
            }
        } else {
            eprintln!("{}: {}", Style::info("fix"), fix.description);
        }
    }

    /// Print a section header.
    pub fn header(&self, title: &str) {
        if self.verbosity >= Verbosity::Normal {
            eprintln!();
            eprintln!("{}", Style::bold(title));
        }
    }

    /// Print a list item.
    pub fn list_item(&self, key: &str, value: &str) {
        if self.verbosity >= Verbosity::Normal {
            eprintln!("  {}: {}", Style::dim(key), value);
        }
    }

    /// Print a success summary with duration.
    pub fn success_summary(&self, action: &str, duration: std::time::Duration) {
        if self.verbosity >= Verbosity::Normal {
            eprintln!(
                "{} {} {}",
                Style::success("✓"),
                action,
                Style::dim(format!("({})", Style::duration(duration)))
            );
        }
    }

    /// Flush stdout.
    pub fn flush(&self) {
        let _ = io::stdout().flush();
        let _ = io::stderr().flush();
    }
}
