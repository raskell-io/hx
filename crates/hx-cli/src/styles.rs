//! CLI styling for help output.
//!
//! This module defines custom styles for clap's help output, providing
//! consistent colored output similar to uv's approach.

use clap::builder::{styling::AnsiColor, Styles};

/// Custom styles for the hx CLI.
///
/// Uses:
/// - Green for headers and usage
/// - Cyan for literals (commands, flags)
/// - Cyan for placeholders
pub const STYLES: Styles = Styles::styled()
    .header(AnsiColor::Green.on_default().bold())
    .usage(AnsiColor::Green.on_default().bold())
    .literal(AnsiColor::Cyan.on_default().bold())
    .placeholder(AnsiColor::Cyan.on_default());
