//! Warning system for hx.
//!
//! Provides macros for emitting user-facing warnings with consistent formatting
//! and optional de-duplication.
//!
//! # Usage
//!
//! ```rust,ignore
//! use hx_warnings::{warn_user, warn_user_once};
//!
//! // Enable warnings (typically done once at startup)
//! hx_warnings::enable();
//!
//! // Emit a warning
//! warn_user!("This is a warning about {}", "something");
//!
//! // Emit a warning only once (de-duplicated by message content)
//! warn_user_once!("This warning will only appear once");
//! ```

use std::collections::HashSet;
use std::error::Error;
use std::iter;
use std::sync::atomic::AtomicBool;
use std::sync::{LazyLock, Mutex};

pub use anstream;
pub use owo_colors;
use owo_colors::{DynColor, OwoColorize, Style};

/// Global flag to enable/disable warnings.
pub static ENABLED: AtomicBool = AtomicBool::new(false);

/// Set of warnings that have already been emitted (for de-duplication).
pub static WARNINGS: LazyLock<Mutex<HashSet<String>>> =
    LazyLock::new(|| Mutex::new(HashSet::new()));

/// Enable warning output.
pub fn enable() {
    ENABLED.store(true, std::sync::atomic::Ordering::Relaxed);
}

/// Disable warning output.
pub fn disable() {
    ENABLED.store(false, std::sync::atomic::Ordering::Relaxed);
}

/// Check if warnings are enabled.
pub fn is_enabled() -> bool {
    ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

/// Emit a warning to stderr.
///
/// The warning is only shown if warnings are enabled via [`enable()`].
#[macro_export]
macro_rules! warn_user {
    ($($arg:tt)*) => {{
        use $crate::anstream::eprintln;
        use $crate::owo_colors::OwoColorize;

        if $crate::ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            let message = format!("{}", format_args!($($arg)*));
            eprintln!("{}{} {}", "warning".yellow().bold(), ":".bold(), message.bold());
        }
    }};
}

/// Emit a warning to stderr, but only once per unique message.
///
/// Subsequent calls with the same message content will be silently ignored.
/// The warning is only shown if warnings are enabled via [`enable()`].
#[macro_export]
macro_rules! warn_user_once {
    ($($arg:tt)*) => {{
        use $crate::anstream::eprintln;
        use $crate::owo_colors::OwoColorize;

        if $crate::ENABLED.load(std::sync::atomic::Ordering::Relaxed) {
            if let Ok(mut states) = $crate::WARNINGS.lock() {
                let message = format!("{}", format_args!($($arg)*));
                if states.insert(message.clone()) {
                    eprintln!("{}{} {}", "warning".yellow().bold(), ":".bold(), message.bold());
                }
            }
        }
    }};
}

/// Write a formatted error chain to a stream.
///
/// This produces output like:
/// ```text
/// error: Failed to build project
///   Caused by: GHC compilation failed
///   Caused by: Module 'Data.Text' not found
/// ```
pub fn write_error_chain(
    err: &dyn Error,
    mut stream: impl std::fmt::Write,
    level: impl AsRef<str>,
    color: impl DynColor + Copy,
) -> std::fmt::Result {
    let style = Style::new().color(color).bold();

    writeln!(
        &mut stream,
        "{}{} {}",
        level.as_ref().style(style),
        ":".bold(),
        err.to_string().trim()
    )?;

    // Walk the error chain
    for source in iter::successors(err.source(), |&err| err.source()) {
        let msg = source.to_string();
        let mut lines = msg.lines();
        if let Some(first) = lines.next() {
            writeln!(
                &mut stream,
                "  {}{} {}",
                "Caused by".style(style),
                ":".bold(),
                first.trim()
            )?;
            // Handle multiline error messages with proper indentation
            for line in lines {
                writeln!(&mut stream, "             {}", line.trim_end())?;
            }
        }
    }

    Ok(())
}

/// Format an error chain to a string.
pub fn format_error_chain(
    err: &dyn Error,
    level: impl AsRef<str>,
    color: impl DynColor + Copy,
) -> String {
    let mut output = String::new();
    let _ = write_error_chain(err, &mut output, level, color);
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enable_disable() {
        disable();
        assert!(!is_enabled());
        enable();
        assert!(is_enabled());
        disable();
        assert!(!is_enabled());
    }
}
