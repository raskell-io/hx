//! Spinner utilities for long-running operations.

use indicatif::{ProgressBar, ProgressStyle};
use std::time::Duration;

/// A spinner for indeterminate progress.
pub struct Spinner {
    bar: ProgressBar,
}

impl Spinner {
    /// Create a new spinner with a message.
    pub fn new(message: impl Into<String>) -> Self {
        let bar = ProgressBar::new_spinner();
        bar.set_style(
            ProgressStyle::default_spinner()
                .tick_chars("⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")
                .template("{spinner:.cyan} {msg}")
                .expect("valid template"),
        );
        bar.set_message(message.into());
        bar.enable_steady_tick(Duration::from_millis(80));
        Self { bar }
    }

    /// Update the spinner message.
    pub fn set_message(&self, message: impl Into<String>) {
        self.bar.set_message(message.into());
    }

    /// Finish the spinner with a success message.
    pub fn finish_success(self, message: impl Into<String>) {
        self.bar.set_style(
            ProgressStyle::default_spinner()
                .template("{msg}")
                .expect("valid template"),
        );
        self.bar.finish_with_message(format!(
            "{} {}",
            console::style("✓").green(),
            message.into()
        ));
    }

    /// Finish the spinner with a failure message.
    pub fn finish_error(self, message: impl Into<String>) {
        self.bar.set_style(
            ProgressStyle::default_spinner()
                .template("{msg}")
                .expect("valid template"),
        );
        self.bar
            .finish_with_message(format!("{} {}", console::style("✗").red(), message.into()));
    }

    /// Finish the spinner with a warning message.
    pub fn finish_warning(self, message: impl Into<String>) {
        self.bar.set_style(
            ProgressStyle::default_spinner()
                .template("{msg}")
                .expect("valid template"),
        );
        self.bar.finish_with_message(format!(
            "{} {}",
            console::style("⚠").yellow(),
            message.into()
        ));
    }

    /// Finish and clear the spinner.
    pub fn finish_clear(self) {
        self.bar.finish_and_clear();
    }
}

/// A progress bar for determinate progress.
pub struct Progress {
    bar: ProgressBar,
}

impl Progress {
    /// Create a new progress bar.
    pub fn new(total: u64, message: impl Into<String>) -> Self {
        let bar = ProgressBar::new(total);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("{msg} [{bar:40.cyan/dim}] {pos}/{len}")
                .expect("valid template")
                .progress_chars("━━─"),
        );
        bar.set_message(message.into());
        Self { bar }
    }

    /// Increment the progress.
    pub fn inc(&self, delta: u64) {
        self.bar.inc(delta);
    }

    /// Set the current position.
    pub fn set_position(&self, pos: u64) {
        self.bar.set_position(pos);
    }

    /// Finish the progress bar.
    pub fn finish(self, message: impl Into<String>) {
        self.bar.finish_with_message(format!(
            "{} {}",
            console::style("✓").green(),
            message.into()
        ));
    }
}
