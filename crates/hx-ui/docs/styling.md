# Terminal Styling

`hx-ui` provides consistent styling for terminal output.

## Style Helper

The `Style` struct provides helper methods for common styles:

```rust
use hx_ui::Style;

// Error (red, bold)
println!("{}", Style::error("Build failed"));

// Warning (yellow)
println!("{}", Style::warning("Deprecated API"));

// Success (green)
println!("{}", Style::success("Build complete"));

// Info (cyan)
println!("{}", Style::info("Compiling Main.hs"));

// Command hint (cyan, bold)
println!("Run {}", Style::command("hx build"));

// Dim (secondary info)
println!("{}", Style::dim("(cached)"));

// Bold
println!("{}", Style::bold("Important"));
```

## Color Semantics

Follow these conventions for consistent output:

| Color | Meaning | Usage |
|-------|---------|-------|
| Green | Success | Checkmarks, completion messages |
| Red | Error | Error messages, failures |
| Yellow | Warning | Warnings, deprecations |
| Cyan | Info | Status messages, commands |
| Dim | Secondary | Timing, cache info |
| Bold | Emphasis | Important info, headers |

## Duration Formatting

Format durations in human-readable form:

```rust
use hx_ui::Style;
use std::time::Duration;

println!("{}", Style::duration(Duration::from_millis(50)));   // "50ms"
println!("{}", Style::duration(Duration::from_secs(2)));      // "2.0s"
println!("{}", Style::duration(Duration::from_secs(90)));     // "1.5m"
```

## Color Environment

Colors respect standard environment variables:

```rust
use hx_ui::colors_enabled;

if colors_enabled() {
    println!("{}", Style::success("Colored output"));
} else {
    println!("Plain output");
}
```

### Environment Variables

| Variable | Effect |
|----------|--------|
| `NO_COLOR` | Disables colors (any value) |
| `CLICOLOR=0` | Disables colors |
| `CLICOLOR_FORCE=1` | Forces colors even if not TTY |

### Non-TTY Detection

Colors are automatically disabled when:
- Output is piped to another command
- Output is redirected to a file
- Running in a CI environment without `CLICOLOR_FORCE`

## Integration with Output

Use with the `Output` struct for consistent messaging:

```rust
use hx_ui::{Output, Style};

let output = Output::new();

// Status messages include built-in styling
output.status("Building", "my-project");

// For custom messages, use Style directly
eprintln!(
    "{} {} {}",
    Style::success("✓"),
    "Compiled",
    Style::dim("(2.3s)")
);
```

## Composing Styles

Styles can be combined:

```rust
use console::style;

// Bold and green
println!("{}", style("Success").green().bold());

// Dim and italic
println!("{}", style("Note").dim().italic());
```

## Best Practices

### 1. Don't Over-Style

```rust
// Good - minimal styling
output.status("Building", project_name);
output.info(&format!("Compiled {} modules", count));

// Bad - too much color
println!("{} {} {} {}",
    Style::success("✓"),
    Style::info("Built"),
    Style::bold(project_name),
    Style::dim(&format!("({} modules)", count))
);
```

### 2. Consistent Error Formatting

```rust
// Good - consistent error format
output.error("Build failed: type error in Main.hs");

// Or structured:
eprintln!("{}", Style::error("error:"));
eprintln!("  Build failed");
eprintln!("  {}", Style::dim("at Main.hs:15:10"));
```

### 3. Use Dim for Metadata

```rust
// Timing, cache info, etc. should be dim
println!("{} {} {}",
    Style::success("✓"),
    "Built my-project",
    Style::dim("(cached)")
);
```

### 4. Commands Should Stand Out

```rust
// Make commands copy-pasteable and visible
println!("Run {} to fix", Style::command("hx toolchain install"));
```

### 5. Test Without Colors

Ensure output is understandable without colors:

```bash
NO_COLOR=1 hx build
```

## Example: Build Output

```rust
use hx_ui::{Output, Style};
use std::time::Duration;

fn print_build_result(output: &Output, packages: &[Package], duration: Duration) {
    output.header("Build Summary");

    for pkg in packages {
        let status = if pkg.cached {
            format!("{}", Style::dim("(cached)"))
        } else {
            format!("{}", Style::dim(&format!("({})", Style::duration(pkg.compile_time))))
        };

        eprintln!(
            "  {} {} {}",
            Style::success("✓"),
            pkg.name,
            status
        );
    }

    eprintln!();
    eprintln!(
        "{} Built {} packages in {}",
        Style::success("✓"),
        packages.len(),
        Style::duration(duration)
    );
}
```

Output:
```
=== Build Summary ===
  ✓ base (cached)
  ✓ text (1.2s)
  ✓ aeson (3.4s)
  ✓ my-project (0.8s)

✓ Built 4 packages in 5.4s
```

## API Reference

### Style

```rust
impl Style {
    /// Style text as error (red, bold).
    pub fn error<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as warning (yellow).
    pub fn warning<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as success (green).
    pub fn success<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as info (cyan).
    pub fn info<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as a command hint (cyan, bold).
    pub fn command<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as dim (secondary info).
    pub fn dim<D: Display>(text: D) -> StyledObject<D>;

    /// Style text as bold.
    pub fn bold<D: Display>(text: D) -> StyledObject<D>;

    /// Format a duration for display.
    pub fn duration(duration: Duration) -> String;
}
```

### Color Detection

```rust
/// Check if colors should be used based on environment.
pub fn colors_enabled() -> bool;
```
