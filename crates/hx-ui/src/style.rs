//! Styling utilities for terminal output.

use console::{StyledObject, style};

/// Style helper for consistent terminal output.
pub struct Style;

impl Style {
    /// Style text as an error (red).
    pub fn error<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).red().bold()
    }

    /// Style text as a warning (yellow).
    pub fn warning<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).yellow()
    }

    /// Style text as success (green).
    pub fn success<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).green()
    }

    /// Style text as info (cyan).
    pub fn info<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).cyan()
    }

    /// Style text as a command hint (cyan, bold).
    pub fn command<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).cyan().bold()
    }

    /// Style text as dim (for secondary info).
    pub fn dim<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).dim()
    }

    /// Style text as bold.
    pub fn bold<D: std::fmt::Display>(text: D) -> StyledObject<D> {
        style(text).bold()
    }

    /// Style a duration in human-readable format.
    pub fn duration(duration: std::time::Duration) -> String {
        let secs = duration.as_secs_f64();
        if secs < 1.0 {
            format!("{:.0}ms", duration.as_millis())
        } else if secs < 60.0 {
            format!("{:.1}s", secs)
        } else {
            let mins = secs / 60.0;
            format!("{:.1}m", mins)
        }
    }
}

/// Check if colors should be used based on environment.
pub fn colors_enabled() -> bool {
    // Respect NO_COLOR standard
    if std::env::var("NO_COLOR").is_ok() {
        return false;
    }

    // Check CLICOLOR
    if let Ok(val) = std::env::var("CLICOLOR")
        && val == "0"
    {
        return false;
    }

    // Default to checking if stdout is a tty
    console::Term::stdout().is_term()
}
