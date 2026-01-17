# Error Chain Formatting

`hx-warnings` provides utilities for formatting error chains with full cause information.

## Overview

When errors are wrapped, each layer adds context. The error chain formatter walks through all causes and displays them in a readable format.

## Basic Usage

### write_error_chain

Write an error chain to any `Write` implementation:

```rust
use hx_warnings::write_error_chain;
use owo_colors::colors::Red;

let error = load_config().unwrap_err();
let mut output = String::new();

write_error_chain(&error, &mut output, "error", Red)?;
eprintln!("{}", output);
```

Output:
```
error: failed to load config

Caused by:
    0: failed to parse TOML
    1: invalid key at line 5
```

### format_error_chain

Get the formatted chain as a String:

```rust
use hx_warnings::format_error_chain;
use owo_colors::colors::Red;

let error = some_operation().unwrap_err();
let formatted = format_error_chain(&error, "error", Red);
eprintln!("{}", formatted);
```

## Customizing the Level

Use different labels for different error types:

```rust
// For regular errors
format_error_chain(&err, "error", Red);
// error: message
// Caused by: ...

// For warnings
format_error_chain(&err, "warning", Yellow);
// warning: message
// Caused by: ...

// For internal errors
format_error_chain(&err, "internal error", Red);
// internal error: message
// Caused by: ...
```

## How It Works

The formatter:

1. Prints the top-level error with the specified label
2. Walks the error's `source()` chain
3. Numbers each cause for clarity
4. Handles multi-line error messages with proper indentation

### Example Chain

```rust
// Original error
io::Error("permission denied")

// Wrapped once
Error::io("failed to read file", path, io_error)

// Wrapped again
Error::config("failed to load config", config_path, wrapped_error)
```

Output:
```
error: failed to load config
  Caused by: failed to read file: /path/to/config
  Caused by: permission denied (os error 13)
```

## Multi-line Errors

Errors with multi-line messages are indented properly:

```rust
// Error with multi-line message
#[derive(thiserror::Error)]
#[error("version conflict for {package}\n  Required: {required}\n  Selected: {selected}")]
struct ConflictError { ... }
```

Output:
```
error: version conflict for aeson
             Required: >= 2.0
             Selected: 1.5.0
  Caused by: constraint added by servant
```

## Integration with thiserror

Works seamlessly with `thiserror` error chains:

```rust
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("failed to read config at {path}")]
    ReadError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("failed to parse config")]
    ParseError {
        #[source]
        source: toml::de::Error,
    },
}

// Usage
let result = load_config();
if let Err(e) = result {
    eprintln!("{}", format_error_chain(&e, "error", Red));
}
```

## Integration with anyhow

Also works with `anyhow` error chains:

```rust
use anyhow::{Context, Result};

fn load_config() -> Result<Config> {
    let content = std::fs::read_to_string(&path)
        .context("failed to read config file")?;

    let config: Config = toml::from_str(&content)
        .context("failed to parse config")?;

    Ok(config)
}

// Display with chain
if let Err(e) = load_config() {
    eprintln!("{}", format_error_chain(e.as_ref(), "error", Red));
}
```

## Best Practices

### 1. Always Include Source Errors

```rust
// Good - preserves the cause
#[error("failed to load config")]
ConfigLoad {
    #[source]
    source: io::Error,
}

// Bad - loses the cause
#[error("failed to load config: {0}")]
ConfigLoad(String);
```

### 2. Add Meaningful Context

```rust
// Good - context at each layer
fs::read_to_string(path)
    .map_err(|e| Error::io(&format!("reading {}", path.display()), e))?
    .parse()
    .map_err(|e| Error::parse(&format!("parsing {}", path.display()), e))?;
```

### 3. Use for User-Facing Output

```rust
// For user-facing errors, show the full chain
if let Err(e) = run_command() {
    eprintln!("{}", format_error_chain(&e, "error", Red));
    std::process::exit(1);
}
```

### 4. For Debug Output, Add More Detail

```rust
// Debug mode: show everything
if verbose {
    eprintln!("Debug error details:");
    eprintln!("{:#?}", error);
}

// Normal mode: formatted chain
eprintln!("{}", format_error_chain(&error, "error", Red));
```

## Example: Full Error Handling

```rust
use hx_warnings::{format_error_chain, warn_user};
use owo_colors::colors::{Red, Yellow};

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => {
            // Check if it's a recoverable error
            if let Some(warning) = e.as_warning() {
                eprintln!("{}", format_error_chain(&warning, "warning", Yellow));
                // Continue with degraded functionality
            } else {
                eprintln!("{}", format_error_chain(&e, "error", Red));
                std::process::exit(1);
            }
        }
    }
}
```

## API Reference

### write_error_chain

```rust
/// Write a formatted error chain to a stream.
pub fn write_error_chain(
    err: &dyn Error,
    stream: impl fmt::Write,
    level: impl AsRef<str>,
    color: impl DynColor + Copy,
) -> fmt::Result;
```

### format_error_chain

```rust
/// Format an error chain to a string.
pub fn format_error_chain(
    err: &dyn Error,
    level: impl AsRef<str>,
    color: impl DynColor + Copy,
) -> String;
```
