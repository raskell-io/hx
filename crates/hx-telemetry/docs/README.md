# hx-telemetry

Structured logging and tracing for hx.

## Overview

`hx-telemetry` sets up the tracing infrastructure:

- Structured logging with `tracing`
- Configurable verbosity levels
- JSON output for debugging
- Timing instrumentation

## Quick Start

```rust
use hx_telemetry::init;

// Initialize based on verbosity
init(verbose)?;

// Now use tracing macros
tracing::info!("Starting build");
tracing::debug!("Cache hit for package");
```

## Initialization

```rust
use hx_telemetry::init;

// Normal mode: WARN level
init(false)?;

// Verbose mode: DEBUG level
init(true)?;
```

## Environment Variables

| Variable | Effect |
|----------|--------|
| `RUST_LOG` | Override log level filter |
| `HX_LOG_JSON` | Enable JSON output format |
| `HX_LOG_FILE` | Write logs to file |

### RUST_LOG Examples

```bash
# All debug logs
RUST_LOG=debug hx build

# Only hx crates at debug
RUST_LOG=hx=debug hx build

# Specific module
RUST_LOG=hx_toolchain::detect=trace hx build
```

## Log Levels

| Level | Usage |
|-------|-------|
| `error!` | Unrecoverable failures |
| `warn!` | Recoverable issues, deprecations |
| `info!` | High-level operation progress |
| `debug!` | Detailed operation info |
| `trace!` | Very verbose debugging |

## Tracing Spans

Use spans for structured context:

```rust
use tracing::{info, info_span};

let _span = info_span!("build", package = %name).entered();
info!("Starting compilation");
// ... build logic ...
info!("Compilation complete");
```

Output:
```
2024-01-15T10:30:00Z INFO build{package=my-lib}: Starting compilation
2024-01-15T10:30:05Z INFO build{package=my-lib}: Compilation complete
```

## Instrumentation

Use `#[tracing::instrument]` for automatic spans:

```rust
#[tracing::instrument(skip(config))]
pub async fn build(config: &Config) -> Result<()> {
    info!("Building project");
    // Automatically creates span with function name
}
```

## Timing

### TimingGuard

```rust
use hx_telemetry::TimingGuard;

let _timer = TimingGuard::new("build");
// ... operation ...
// Logs duration when dropped
```

### time! Macro

```rust
use hx_telemetry::time;

time!("resolve_deps", {
    resolver.resolve(&constraints)?
});
// Logs: resolve_deps completed in 1.23s
```

## JSON Output

Enable JSON for machine-readable logs:

```bash
HX_LOG_JSON=1 hx build
```

Output:
```json
{"timestamp":"2024-01-15T10:30:00Z","level":"INFO","target":"hx_cli","message":"Building my-project"}
{"timestamp":"2024-01-15T10:30:05Z","level":"INFO","target":"hx_cli","message":"Build complete","duration_ms":5000}
```

## File Logging

Log to file for debugging:

```bash
HX_LOG_FILE=/tmp/hx.log hx build
```

## Integration with Output

`hx-telemetry` works alongside `hx-ui`:

- **hx-ui** - User-facing output (status, progress, errors)
- **hx-telemetry** - Developer/debug logging

```rust
// User sees this
output.status("Building", "my-project");

// Developers see this (with RUST_LOG=debug)
tracing::debug!("Build options: {:?}", options);
```

## Best Practices

1. **Use spans for context** - Group related operations
2. **Skip sensitive data** - `#[instrument(skip(password))]`
3. **Use appropriate levels** - Debug for details, Info for progress
4. **Include relevant fields** - Package names, versions, paths
5. **Don't log user-facing messages** - Use hx-ui instead

## Documentation

- [Tracing Spans](./spans.md) - Detailed guide to using spans
