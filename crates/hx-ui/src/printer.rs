//! Printer abstraction for controlling output verbosity.
//!
//! This module provides a `Printer` type that controls what output is shown
//! based on verbosity level, similar to uv's approach.

use indicatif::ProgressDrawTarget;

/// Output verbosity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Printer {
    /// Suppress all output.
    Silent,
    /// Suppress most output but preserve important stdout.
    Quiet,
    /// Normal output (default).
    #[default]
    Normal,
    /// Normal output but exclude progress bars.
    NoProgress,
    /// Verbose output including debug messages.
    Verbose,
}

impl Printer {
    /// Create a printer from quiet/verbose flags.
    pub fn from_flags(quiet: u8, verbose: bool) -> Self {
        if quiet >= 2 {
            Self::Silent
        } else if quiet == 1 {
            Self::Quiet
        } else if verbose {
            Self::Verbose
        } else {
            Self::Normal
        }
    }

    /// Get the progress bar draw target for this printer.
    pub fn target(self) -> ProgressDrawTarget {
        match self {
            Self::Silent | Self::Quiet | Self::Verbose | Self::NoProgress => {
                ProgressDrawTarget::hidden()
            }
            Self::Normal => ProgressDrawTarget::stderr(),
        }
    }

    /// Check if stdout output is enabled.
    pub fn stdout_enabled(self) -> bool {
        matches!(self, Self::Normal | Self::Verbose | Self::NoProgress)
    }

    /// Check if stderr output is enabled.
    pub fn stderr_enabled(self) -> bool {
        !matches!(self, Self::Silent)
    }

    /// Check if progress output is enabled.
    pub fn progress_enabled(self) -> bool {
        matches!(self, Self::Normal)
    }

    /// Check if verbose output is enabled.
    pub fn is_verbose(self) -> bool {
        matches!(self, Self::Verbose)
    }

    /// Check if quiet mode is active.
    pub fn is_quiet(self) -> bool {
        matches!(self, Self::Quiet | Self::Silent)
    }
}

/// Standard output control.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stdout {
    Enabled,
    Disabled,
}

impl From<Printer> for Stdout {
    fn from(printer: Printer) -> Self {
        if printer.stdout_enabled() {
            Self::Enabled
        } else {
            Self::Disabled
        }
    }
}
