# Auto-Install

hx automatically installs required toolchain versions when needed.

## Overview

When `hx build` runs and the project's `hx.toml` specifies toolchain versions, hx will:

1. Detect currently installed tools
2. Compare with required versions
3. Automatically install missing/mismatched versions
4. Continue with the build

## Example Flow

```bash
$ hx build
   Downloading GHC 9.8.2...
   Downloading Cabal 3.12.1.0...
    Building my-project
```

No manual `hx toolchain install` required!

## AutoInstallPolicy

```rust
pub enum AutoInstallPolicy {
    /// Never auto-install, fail if missing
    Never,
    /// Prompt user before installing
    OnDemand,
    /// Always install without prompting
    Always,
}
```

## ensure_toolchain Function

```rust
use hx_toolchain::{Toolchain, ensure_toolchain, AutoInstallPolicy};

let toolchain = Toolchain::detect().await;

// Ensure GHC 9.8.2 and Cabal 3.12.1.0 are available
ensure_toolchain(
    &toolchain,
    Some("9.8.2"),      // Required GHC (from hx.toml)
    Some("3.12.1.0"),   // Required Cabal (from hx.toml)
    AutoInstallPolicy::Always,
).await?;
```

### Behavior by Policy

| Policy | Missing Tool | Wrong Version |
|--------|--------------|---------------|
| `Never` | Error | Error |
| `OnDemand` | Prompt | Prompt |
| `Always` | Install | Install |

## Project-Specific Toolchains

When `hx.toml` specifies versions:

```toml
[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
```

The effective policy becomes `Always` (install silently).

```rust
// In build command implementation
let effective_policy = if project.manifest.toolchain.ghc.is_some() {
    AutoInstallPolicy::Always
} else {
    policy  // Use user-specified policy
};
```

## Disabling Auto-Install

Use `--no-auto-install` flag:

```bash
$ hx build --no-auto-install
error: toolchain not found: ghc 9.8.2

fix: Run `hx toolchain install --ghc 9.8.2`
```

## Re-Detection After Install

After installation, toolchains must be re-detected:

```rust
let mut toolchain = Toolchain::detect().await;

// Install if needed
ensure_toolchain(&toolchain, Some("9.8.2"), Some("3.12.1.0"), policy).await?;

// Re-detect to get updated paths
if !toolchain.ghc.status.is_found() || !toolchain.cabal.status.is_found() {
    toolchain = Toolchain::detect().await;
}

// Now toolchain has correct paths
let ghc_path = toolchain.ghc.status.path().unwrap();
```

## Collecting Toolchain Bin Directories

After ensuring toolchain, collect bin dirs for PATH:

```rust
let mut toolchain_bin_dirs = Vec::new();

if let Some(ghc_path) = toolchain.ghc.status.path() {
    if let Some(parent) = ghc_path.parent() {
        toolchain_bin_dirs.push(parent.to_path_buf());
    }
}

if let Some(cabal_path) = toolchain.cabal.status.path() {
    if let Some(parent) = cabal_path.parent() {
        toolchain_bin_dirs.push(parent.to_path_buf());
    }
}

// Pass to build commands
build(&project, &toolchain_bin_dirs).await?;
```

## Error Handling

```rust
match ensure_toolchain(&toolchain, Some("9.8.2"), None, policy).await {
    Ok(()) => println!("Toolchain ready"),
    Err(e) => {
        // Error includes fix suggestions
        output.print_error(&e);
        // Output:
        // error: toolchain not found: ghc 9.8.2
        // fix: Run `hx toolchain install --ghc 9.8.2`
        return Ok(ExitCode::ToolchainError.into());
    }
}
```

## User Experience

### First Time Setup

```bash
$ cd my-haskell-project
$ hx build
   Downloading GHC 9.8.2 (245 MB)...
   [========================================] 100%
   Installing GHC 9.8.2...
   Downloading Cabal 3.12.1.0 (12 MB)...
   [========================================] 100%
    Building my-project
    Finished in 45.2s
```

### Subsequent Builds

```bash
$ hx build
    Building my-project
    Finished in 2.3s
```

Toolchains are cached and reused.

## CI/CD Integration

For CI, auto-install ensures reproducible builds:

```yaml
# GitHub Actions
- name: Build
  run: |
    cargo install hx
    hx build
```

No need to pre-install GHC - hx handles it.

## Best Practices

1. **Always specify toolchain versions** in `hx.toml` for reproducibility
2. **Use auto-install** for seamless developer experience
3. **Cache `~/.hx/toolchains`** in CI for faster builds
4. **Don't commit** toolchain binaries to version control
