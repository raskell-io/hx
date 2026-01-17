# Command Runner

The `CommandRunner` provides a safe, cross-platform way to execute external processes.

## Overview

```rust
use hx_core::{CommandRunner, CommandOutput};

let runner = CommandRunner::new();
let output = runner.run("ghc", &["--version"]).await?;
```

## CommandRunner

```rust
pub struct CommandRunner {
    /// Working directory for the command
    pub working_dir: Option<PathBuf>,
    /// Environment variables to set
    pub env: Vec<(String, String)>,
    /// Whether to inherit parent environment
    pub inherit_env: bool,
}
```

### Creating a Runner

```rust
// Default runner (inherits environment, uses current directory)
let runner = CommandRunner::new();

// With custom working directory
let runner = CommandRunner::new()
    .working_dir("/path/to/project");

// With custom environment
let runner = CommandRunner::new()
    .env("GHC_PACKAGE_PATH", "/path/to/packages")
    .env("HOME", "/home/user");

// Without inheriting parent environment
let runner = CommandRunner::new()
    .no_inherit_env()
    .env("PATH", "/usr/bin");
```

### Adding Toolchain to PATH

The `with_ghc_bin` method prepends a directory to PATH:

```rust
// Add GHC bin directory to PATH
let runner = CommandRunner::new()
    .with_ghc_bin("/home/user/.hx/toolchains/ghc/9.8.2/bin");

// Multiple bin directories (accumulates)
let runner = CommandRunner::new()
    .with_ghc_bin("/path/to/ghc/bin")
    .with_ghc_bin("/path/to/cabal/bin");
```

This is essential for running hx-managed toolchains that aren't in the system PATH.

### Running Commands

```rust
// Basic execution
let output = runner.run("ghc", &["--version"]).await?;

// With arguments
let output = runner.run("cabal", &["build", "--jobs=4"]).await?;

// Check success
if output.exit_code == 0 {
    println!("Success: {}", output.stdout);
} else {
    eprintln!("Failed: {}", output.stderr);
}
```

## CommandOutput

```rust
pub struct CommandOutput {
    /// Process exit code
    pub exit_code: i32,
    /// Captured stdout
    pub stdout: String,
    /// Captured stderr
    pub stderr: String,
    /// Execution duration
    pub duration: Duration,
}
```

### Checking Results

```rust
let output = runner.run("ghc", &["--make", "Main.hs"]).await?;

// Check exit code
if output.exit_code != 0 {
    return Err(Error::command_failed("ghc --make", &output));
}

// Access output
println!("Compiled in {:?}", output.duration);
println!("Warnings: {}", output.stderr);
```

## Cross-Platform Considerations

### PATH Separator

The runner handles PATH separators automatically:

- Unix: `/path/one:/path/two`
- Windows: `C:\path\one;C:\path\two`

### Executable Extensions

On Windows, `.exe` is handled automatically when searching PATH.

### Working Directory

Always use `PathBuf` for paths - the runner handles platform differences.

## Error Handling

When a command fails, create a structured error:

```rust
let output = runner.run("cabal", &["build"]).await?;

if output.exit_code != 0 {
    return Err(Error::CommandFailed {
        command: "cabal build".into(),
        exit_code: Some(output.exit_code),
        stdout: output.stdout,
        stderr: output.stderr,
        fixes: vec![
            Fix::command("Check dependencies", "hx lock"),
            Fix::command("Clean and rebuild", "hx clean && hx build"),
        ],
    });
}
```

## Streaming Output

For long-running commands where you want real-time output:

```rust
let output = runner
    .run_streaming("cabal", &["build"], |line| {
        println!("{}", line);
    })
    .await?;
```

## Timeouts

Set a timeout for command execution:

```rust
let output = runner
    .with_timeout(Duration::from_secs(300))
    .run("cabal", &["build"])
    .await?;
```

## Best Practices

1. **Always set working directory** for project-specific commands
2. **Use `with_ghc_bin`** for hx-managed toolchains
3. **Capture both stdout and stderr** for error reporting
4. **Include the command in errors** so users know what failed
5. **Use structured errors** with fix suggestions
