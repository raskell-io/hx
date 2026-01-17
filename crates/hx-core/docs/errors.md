# Error Types

`hx-core` provides structured error types that ensure every failure includes actionable information.

## Error Enum

The main `Error` enum categorizes all possible failures:

```rust
pub enum Error {
    /// Required tool not found in PATH
    ToolchainMissing {
        tool: String,
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    /// Tool found but wrong version
    ToolchainMismatch {
        tool: String,
        expected: String,
        found: String,
        fixes: Vec<Fix>,
    },

    /// Configuration file error
    Config {
        message: String,
        path: Option<PathBuf>,
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    /// File system error
    Io {
        message: String,
        path: Option<PathBuf>,
        source: std::io::Error,
    },

    /// External command failed
    CommandFailed {
        command: String,
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
        fixes: Vec<Fix>,
    },

    /// Build compilation failed
    BuildFailed {
        errors: Vec<String>,
        fixes: Vec<Fix>,
    },

    /// Lockfile operation failed
    Lock {
        message: String,
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    /// No hx project found
    ProjectNotFound {
        searched: Vec<PathBuf>,
        fixes: Vec<Fix>,
    },

    /// Catch-all for other errors
    Other(anyhow::Error),
}
```

## Error Codes

Each error maps to a category for programmatic handling:

```rust
pub enum ErrorCode {
    ToolchainMissing,   // Required tool not installed
    ToolchainMismatch,  // Wrong version installed
    HlsMismatch,        // HLS incompatible with GHC
    SystemDepMissing,   // Native library missing
    SolverFailure,      // Dependency resolution failed
    BuildFailure,       // Compilation error
    ConfigError,        // Invalid configuration
    IoError,            // File system error
    CommandFailed,      // Process execution failed
    LockError,          // Lockfile error
}
```

## Fix Suggestions

Every error can include suggested fixes:

```rust
pub struct Fix {
    /// Human-readable description
    pub description: String,
    /// Optional command to run
    pub command: Option<String>,
}

impl Fix {
    /// Create a fix with just a description
    pub fn new(description: impl Into<String>) -> Self;

    /// Create a fix with a command
    pub fn command(description: impl Into<String>, cmd: impl Into<String>) -> Self;
}
```

## Creating Errors

Use the builder pattern for errors:

```rust
// Toolchain missing
let err = Error::toolchain_missing("ghc")
    .with_fix(Fix::command(
        "Install GHC with ghcup",
        "ghcup install ghc 9.8.2"
    ))
    .with_fix(Fix::command(
        "Or use hx toolchain",
        "hx toolchain install"
    ));

// Config error
let err = Error::config("invalid version format")
    .with_path(config_path)
    .with_fix(Fix::new("Use format: major.minor.patch"));

// IO error
let err = Error::io("failed to read file", path, io_error);

// Command failed
let err = Error::command_failed("cabal build", output)
    .with_fix(Fix::command("Check dependencies", "hx lock"));
```

## Error Display

Errors format with full context:

```
error: GHC version mismatch
  expected: 9.8.2 (from hx.toml)
  found: 9.6.4

fix: Run `hx toolchain install --ghc 9.8.2`
     Or run `ghcup install ghc 9.8.2 && ghcup set ghc 9.8.2`
```

## Result Type

The crate provides a convenience type alias:

```rust
pub type Result<T> = std::result::Result<T, Error>;
```

## Converting from Other Errors

```rust
// From anyhow
let err: Error = anyhow::anyhow!("something failed").into();

// From io::Error with context
let err = Error::io("reading config", path, io_err);

// From other errors via ?
fn load() -> Result<Config> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| Error::io("reading config", path, e))?;
    Ok(parse(content)?)
}
```

## Best Practices

1. **Always include context** - What operation was attempted?
2. **Always suggest fixes** - What can the user do?
3. **Use specific variants** - Avoid `Error::Other` when possible
4. **Chain errors** - Preserve the source error
5. **Be actionable** - Commands are better than descriptions
