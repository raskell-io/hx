# CLI Design Rules

## Command Structure

```
hx <command> [subcommand] [options] [args]
```

## Output Modes

### Default (Compact)
- Show step title
- Show elapsed time
- High-level summary on failure
- Suitable for operators and terminals

### Verbose (`--verbose`)
- Print underlying tool output
- Show all subprocess commands
- Include timing for each step
- Useful for debugging

## Progress Display

- Use spinners for indeterminate operations
- Use progress bars when total is known
- Clear spinners on completion
- Never leave orphaned spinners on error

```rust
// Use indicatif
let pb = ProgressBar::new_spinner();
pb.set_message("Building...");
pb.enable_steady_tick(Duration::from_millis(100));
// ... operation
pb.finish_with_message("Build complete (2.3s)");
```

## Color Usage

- **Green**: Success, completion
- **Red**: Error
- **Yellow**: Warning, attention needed
- **Cyan**: Info, command hints
- **Bold**: Emphasis, summaries
- Respect `NO_COLOR` and `CLICOLOR` env vars

## Command Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Usage error (bad args) |
| 3 | Configuration error |
| 4 | Toolchain error |
| 5 | Build/test failure |

## Clap Configuration

```rust
#[derive(Parser)]
#[command(name = "hx")]
#[command(about = "Haskell toolchain CLI")]
#[command(version)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    #[arg(short, long, global = true)]
    pub verbose: bool,
}
```

## Argument Conventions

- Use `--flag` for boolean options
- Use `--option <value>` for options with values
- Use `--option=<value>` syntax should also work
- Short flags only for common options (`-v`, `-j`, `-n`)
- Positional args only when unambiguous

## Help Text

- Keep descriptions under 80 chars
- Show examples in command help
- Group related options
- Don't repeat info from parent command

```rust
/// Build the project
#[derive(Args)]
pub struct BuildArgs {
    /// Build in release mode with optimizations
    #[arg(long)]
    pub release: bool,

    /// Number of parallel jobs
    #[arg(short, long, default_value = "4")]
    pub jobs: usize,
}
```

## Interactive Prompts

- Avoid prompts in normal operation
- Use `--yes` / `-y` to skip confirmations
- Detect non-TTY and fail with clear message
- Never prompt in CI (detect `CI=true`)
