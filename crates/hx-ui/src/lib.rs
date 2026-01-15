//! Terminal UI helpers for hx.
//!
//! This crate provides consistent output formatting, spinners, progress bars,
//! and error display for the hx CLI.

pub mod output;
pub mod printer;
pub mod spinner;
pub mod style;

pub use output::{Output, Verbosity};
pub use printer::Printer;
pub use spinner::Spinner;
pub use style::Style;
