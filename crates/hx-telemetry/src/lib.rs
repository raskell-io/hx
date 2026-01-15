//! Telemetry and tracing for hx.
//!
//! This crate provides:
//! - Structured logging setup
//! - Timing measurements
//! - JSON log output for debugging

use tracing_subscriber::{
    EnvFilter,
    fmt::{self, format::FmtSpan},
    prelude::*,
};

/// Initialize the tracing subscriber.
///
/// This sets up logging based on the RUST_LOG environment variable
/// and the verbose flag.
pub fn init(verbose: bool) {
    let filter = if verbose {
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("hx=debug"))
    } else {
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("hx=warn"))
    };

    let subscriber = tracing_subscriber::registry().with(filter);

    // Use JSON format if HX_LOG_JSON is set
    if std::env::var("HX_LOG_JSON").is_ok() {
        let json_layer = fmt::layer()
            .json()
            .with_span_events(FmtSpan::CLOSE)
            .with_target(true)
            .with_file(true)
            .with_line_number(true);

        subscriber.with(json_layer).init();
    } else {
        let fmt_layer = fmt::layer()
            .with_target(false)
            .with_file(false)
            .without_time();

        subscriber.with(fmt_layer).init();
    }
}

/// A timing guard that logs duration on drop.
pub struct TimingGuard {
    name: String,
    start: std::time::Instant,
}

impl TimingGuard {
    /// Start timing an operation.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            start: std::time::Instant::now(),
        }
    }
}

impl Drop for TimingGuard {
    fn drop(&mut self) {
        let duration = self.start.elapsed();
        tracing::debug!(
            operation = %self.name,
            duration_ms = duration.as_millis() as u64,
            "Operation completed"
        );
    }
}

/// Macro to time a block of code.
#[macro_export]
macro_rules! time {
    ($name:expr, $block:expr) => {{
        let _guard = $crate::TimingGuard::new($name);
        $block
    }};
}
