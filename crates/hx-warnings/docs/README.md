# hx-warnings

De-duplicated warning system and error chain formatting.

## Overview

`hx-warnings` provides:

- **De-duplicated warnings** - Same message only shown once
- **Error chain formatting** - Walk cause chain
- **Global enable/disable** - Suppress all warnings

## Quick Start

```rust
use hx_warnings::{warn_user, warn_user_once};

// Standard warning (may repeat)
warn_user!("Deprecated API used");

// De-duplicated warning (only shows once)
warn_user_once!("Config format is deprecated");
warn_user_once!("Config format is deprecated");  // Not shown again
```

## Warning Macros

### warn_user!

Emit a warning to stderr:

```rust
warn_user!("Something unexpected happened");
// Output: warning: Something unexpected happened
```

With formatting:

```rust
warn_user!("Package {} is outdated ({})", name, version);
// Output: warning: Package foo is outdated (1.0.0)
```

### warn_user_once!

Emit a warning only once per session:

```rust
for item in items {
    warn_user_once!("Slow operation, consider caching");
    // Only printed on first iteration
}
```

De-duplication is based on the exact message text.

## Global Control

### Enable/Disable Warnings

```rust
use hx_warnings::{enable, disable, is_enabled};

// Disable all warnings
disable();

warn_user!("This won't be shown");

// Re-enable
enable();

// Check status
if is_enabled() {
    warn_user!("Warnings are enabled");
}
```

## Error Chain Formatting

Format errors with their full cause chain:

### write_error_chain

```rust
use hx_warnings::write_error_chain;
use std::io::stderr;

let error = load_config().unwrap_err();
write_error_chain(&mut stderr(), &error)?;
```

Output:
```
error: failed to load config

Caused by:
    0: failed to parse TOML
    1: invalid key at line 5
```

### format_error_chain

Get the chain as a String:

```rust
use hx_warnings::format_error_chain;

let error = some_operation().unwrap_err();
let formatted = format_error_chain(&error);
eprintln!("{}", formatted);
```

## Warning Output Format

Warnings are styled consistently:

```
warning: message here
^^^^^^^
yellow, bold
```

## Thread Safety

The de-duplication set is thread-safe:

```rust
use std::thread;

// Safe to use from multiple threads
let handles: Vec<_> = (0..10).map(|_| {
    thread::spawn(|| {
        warn_user_once!("Concurrent warning");
    })
}).collect();

for h in handles {
    h.join().unwrap();
}
// Only one warning printed
```

## Integration with hx-ui

Use warnings for non-critical issues:

```rust
use hx_ui::Output;
use hx_warnings::warn_user_once;

let output = Output::new();

// Use Output for errors
output.error("Build failed");

// Use warn_user_once for deprecation warnings
warn_user_once!("The 'native' option is deprecated, use 'experimental.native'");
```

## Best Practices

1. **Use `warn_user_once!` for deprecations** - Avoid spam
2. **Use `warn_user!` for actionable warnings** - May need repeating
3. **Include context** - Package names, file paths
4. **Be concise** - One line if possible
5. **Suggest fixes** - What should the user do?

## Example

```rust
use hx_warnings::{warn_user, warn_user_once};

fn process_config(config: &Config) {
    // Deprecation warning (once per session)
    if config.has_deprecated_field() {
        warn_user_once!(
            "The 'resolver' field is deprecated, use 'build.resolver' instead"
        );
    }

    // Per-item warning (may repeat)
    for dep in &config.dependencies {
        if dep.is_yanked() {
            warn_user!(
                "Dependency {} {} is yanked, consider upgrading",
                dep.name,
                dep.version
            );
        }
    }
}
```
