# Progress Indicators

`hx-ui` provides spinners and progress bars for long-running operations.

## Spinner

Use spinners for operations with unknown duration.

### Basic Usage

```rust
use hx_ui::Spinner;

let spinner = Spinner::new("Downloading GHC 9.8.2...");

// ... long operation ...

spinner.finish_success("Downloaded GHC 9.8.2");
```

Output:
```
⠋ Downloading GHC 9.8.2...
✓ Downloaded GHC 9.8.2
```

### Updating the Message

```rust
let spinner = Spinner::new("Initializing...");

// Later, update the message
spinner.set_message("Downloading dependencies...");
spinner.set_message("Compiling...");

spinner.finish_success("Complete");
```

### Finish States

```rust
// Success (green checkmark)
spinner.finish_success("Operation complete");
// ✓ Operation complete

// Error (red X)
spinner.finish_error("Operation failed");
// ✗ Operation failed

// Warning (yellow warning sign)
spinner.finish_warning("Completed with warnings");
// ⚠ Completed with warnings

// Clear (no message, clean terminal)
spinner.finish_clear();
```

### Spinner Style

The default spinner uses braille characters:

```
⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
```

## Progress Bar

Use progress bars when you know the total amount of work.

### Basic Usage

```rust
use hx_ui::Progress;

let progress = Progress::new(100, "Building");

for i in 0..100 {
    // Do work...
    progress.inc(1);
}

progress.finish("Build complete");
```

Output:
```
Building [━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━] 100/100
✓ Build complete
```

### Setting Position Directly

```rust
let progress = Progress::new(total_files, "Compiling");

for (i, file) in files.iter().enumerate() {
    compile(file);
    progress.set_position((i + 1) as u64);
}
```

### Increment by Varying Amounts

```rust
let progress = Progress::new(total_bytes, "Downloading");

while let Some(chunk) = stream.next().await {
    progress.inc(chunk.len() as u64);
}
```

## Best Practices

### 1. Clear Spinners on Error

```rust
let spinner = Spinner::new("Processing...");

match do_work() {
    Ok(result) => spinner.finish_success("Done"),
    Err(e) => {
        spinner.finish_error("Failed");
        // Then show detailed error
        output.error(&e.to_string());
    }
}
```

### 2. Don't Nest Spinners

Only show one spinner at a time:

```rust
// Bad - nested spinners
let outer = Spinner::new("Building...");
for pkg in packages {
    let inner = Spinner::new(&format!("Building {}...", pkg));
    // Messy output
}

// Good - single spinner with updates
let spinner = Spinner::new("Building...");
for pkg in packages {
    spinner.set_message(&format!("Building {}...", pkg.name));
    build(pkg);
}
spinner.finish_success("Build complete");
```

### 3. Use Progress for Known Totals

```rust
// Good - known total
let progress = Progress::new(packages.len() as u64, "Compiling");
for pkg in packages {
    compile(pkg);
    progress.inc(1);
}

// Bad - use spinner instead
let spinner = Spinner::new("Compiling packages...");
for pkg in packages {
    compile(pkg);  // No progress indication
}
```

### 4. Handle Interrupts

The spinner and progress bar are automatically cleaned up when dropped, but you may want to handle Ctrl+C:

```rust
let spinner = Spinner::new("Working...");

// Set up Ctrl+C handler
ctrlc::set_handler(move || {
    // Spinner will be cleaned up on drop
    std::process::exit(130);
})?;

// Work...
spinner.finish_success("Done");
```

### 5. Non-TTY Output

Progress indicators automatically detect non-TTY output and simplify:

```rust
// In a pipe or CI, output is simplified:
// No animation, just start/end messages
```

## Example: Multi-Stage Operation

```rust
use hx_ui::{Spinner, Progress, Output};

pub async fn install_toolchain(output: &Output) -> Result<()> {
    // Stage 1: Check requirements
    let spinner = Spinner::new("Checking requirements...");
    let checks = check_requirements().await?;
    spinner.finish_success("Requirements OK");

    // Stage 2: Download (known size)
    let progress = Progress::new(download_size, "Downloading GHC");
    let mut downloaded = 0;
    while let Some(chunk) = stream.next().await {
        downloaded += chunk.len();
        progress.set_position(downloaded as u64);
    }
    progress.finish("Download complete");

    // Stage 3: Extract (unknown duration)
    let spinner = Spinner::new("Extracting...");
    extract_archive(&archive_path)?;
    spinner.finish_success("Extraction complete");

    // Stage 4: Configure (unknown duration)
    let spinner = Spinner::new("Configuring...");
    configure_ghc(&install_path)?;
    spinner.finish_success("GHC installed");

    output.status("Installed", "GHC 9.8.2");
    Ok(())
}
```

## API Reference

### Spinner

```rust
impl Spinner {
    /// Create a new spinner with a message.
    pub fn new(message: impl Into<String>) -> Self;

    /// Update the spinner message.
    pub fn set_message(&self, message: impl Into<String>);

    /// Finish with success (green checkmark).
    pub fn finish_success(self, message: impl Into<String>);

    /// Finish with error (red X).
    pub fn finish_error(self, message: impl Into<String>);

    /// Finish with warning (yellow warning sign).
    pub fn finish_warning(self, message: impl Into<String>);

    /// Finish and clear (no output).
    pub fn finish_clear(self);
}
```

### Progress

```rust
impl Progress {
    /// Create a new progress bar.
    pub fn new(total: u64, message: impl Into<String>) -> Self;

    /// Increment the progress.
    pub fn inc(&self, delta: u64);

    /// Set the current position.
    pub fn set_position(&self, pos: u64);

    /// Finish with a message.
    pub fn finish(self, message: impl Into<String>);
}
```
