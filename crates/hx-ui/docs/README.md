# hx-ui

Terminal UI helpers for consistent output formatting.

## Overview

`hx-ui` provides:

- **Output formatting** - Consistent message styling
- **Progress indicators** - Spinners and progress bars
- **Color handling** - Respects NO_COLOR
- **Verbosity levels** - Quiet/Normal/Verbose

## Quick Start

```rust
use hx_ui::{Output, Verbosity};

let output = Output::with_verbosity(Verbosity::Normal);

output.status("Building", "my-project");
output.info("Compiling 5 modules...");
output.warn("Deprecated API used");
output.error("Build failed");
output.verbose("Debug info here");  // Only shown if verbose
```

## Output

The main struct for terminal output:

```rust
pub struct Output {
    verbosity: Verbosity,
}

pub enum Verbosity {
    Quiet,   // Only errors
    Normal,  // Status, info, warnings, errors
    Verbose, // Everything including debug
}

impl Output {
    /// Create with default (Normal) verbosity
    pub fn new() -> Self;

    /// Create with specific verbosity
    pub fn with_verbosity(verbosity: Verbosity) -> Self;

    /// Check if verbose mode
    pub fn is_verbose(&self) -> bool;
}
```

## Message Types

### Status Messages

```rust
output.status("Building", "my-project");
// Output:    Building my-project
//         ^^^ green, bold
```

### Info Messages

```rust
output.info("Compiling Main.hs");
// Output:       info: Compiling Main.hs
//            ^^^ cyan
```

### Warnings

```rust
output.warn("Deprecated function used");
// Output:    warning: Deprecated function used
//         ^^^ yellow
```

### Errors

```rust
output.error("Build failed");
// Output:      error: Build failed
//           ^^^ red
```

### Verbose/Debug

```rust
output.verbose("Cache hit for aeson-2.2.1.0");
// Only printed if Verbosity::Verbose
```

### Headers

```rust
output.header("Dependencies");
// Output: === Dependencies ===
```

## Error Display

Format structured errors with fixes:

```rust
use hx_core::Error;

let error = Error::toolchain_missing("ghc")
    .with_fix(Fix::command("Install GHC", "hx toolchain install"));

output.print_error(&error);
```

Output:
```
error: toolchain not found: ghc

fix: Install GHC
     Run `hx toolchain install`
```

## Progress Indicators

### Spinner

For indeterminate operations:

```rust
use hx_ui::Spinner;

let spinner = Spinner::new("Downloading GHC 9.8.2...");
// ... long operation ...
spinner.finish_with_message("Downloaded GHC 9.8.2");
```

### Progress Bar

For operations with known total:

```rust
use hx_ui::Progress;

let progress = Progress::new(100);
for i in 0..100 {
    progress.set_position(i);
    // ... work ...
}
progress.finish();
```

## Styling

### Style Helpers

```rust
use hx_ui::Style;

println!("{}", Style::success("Build complete"));  // Green
println!("{}", Style::error("Failed"));            // Red
println!("{}", Style::warning("Deprecated"));      // Yellow
println!("{}", Style::info("Info"));               // Cyan
println!("{}", Style::dim("Debug info"));          // Dim
println!("{}", Style::bold("Important"));          // Bold
println!("{}", Style::command("hx build"));        // Styled command
```

### Color Respect

Colors are automatically disabled when:
- `NO_COLOR` environment variable is set
- Output is not a TTY
- `CLICOLOR=0` is set

```rust
// Check if colors are enabled
if console::colors_enabled() {
    // Use colors
} else {
    // Plain text
}
```

## Verbosity Handling

```rust
let output = Output::with_verbosity(Verbosity::Verbose);

// Always shown (Normal+)
output.status("Building", "project");
output.info("Compiling...");
output.warn("Warning!");
output.error("Error!");

// Only in Verbose mode
output.verbose("Debug: cache hit");

// Conditional output
if output.is_verbose() {
    println!("Extra debug information...");
}
```

## CLI Integration

```rust
use clap::Parser;
use hx_ui::{Output, Verbosity};

#[derive(Parser)]
struct Cli {
    #[arg(short, long)]
    verbose: bool,

    #[arg(short, long)]
    quiet: bool,
}

fn main() {
    let cli = Cli::parse();

    let verbosity = if cli.quiet {
        Verbosity::Quiet
    } else if cli.verbose {
        Verbosity::Verbose
    } else {
        Verbosity::Normal
    };

    let output = Output::with_verbosity(verbosity);
    // ...
}
```

## Best Practices

1. **Use `status()` for actions** - Building, Testing, Running
2. **Use `info()` for details** - File names, counts
3. **Use `warn()` sparingly** - Only for actionable warnings
4. **Use `error()` for failures** - With fix suggestions
5. **Use `verbose()` for debug** - Cache hits, timing
6. **Respect NO_COLOR** - Don't force colors

## Documentation

- [Progress Indicators](./progress.md) - Spinners and progress bars
- [Terminal Styling](./styling.md) - Colors and formatting
