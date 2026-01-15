# hx

A fast, opinionated, batteries-included toolchain CLI for Haskell.

[![CI](https://github.com/raskell-io/hx/actions/workflows/ci.yml/badge.svg)](https://github.com/raskell-io/hx/actions/workflows/ci.yml)

## Features

- **Fast feedback loop** - Common workflows are snappy
- **Deterministic builds** - Lockfile + frozen plans for reproducibility
- **Excellent errors** - Actionable messages with fix suggestions
- **Pragmatic adoption** - Wraps existing tools (Cabal, GHCup, fourmolu, hlint)

## Installation

### From source

```bash
cargo install --git https://github.com/raskell-io/hx hx-cli
```

### Prerequisites

hx orchestrates existing Haskell tools. You'll need:

- [GHCup](https://www.haskell.org/ghcup/) - Toolchain manager
- [GHC](https://www.haskell.org/ghc/) - Haskell compiler
- [Cabal](https://www.haskell.org/cabal/) - Build system

Run `hx doctor` to check your setup.

## Quick Start

```bash
# Create a new project
hx init --bin --name myapp
cd myapp

# Build and run
hx build
hx run

# Lock dependencies for reproducibility
hx lock
hx sync
```

## Commands

### Project Management

```bash
hx init              # Initialize a new project
hx init --bin        # Create an executable project
hx init --lib        # Create a library project
hx init --ci         # Include GitHub Actions workflow
```

### Building

```bash
hx build             # Build the project
hx build --release   # Build with optimizations
hx build -j4         # Parallel build with 4 jobs
hx check             # Fast type-check
hx clean             # Clean build artifacts
```

### Running

```bash
hx run               # Build and run the executable
hx run -- arg1 arg2  # Pass arguments to the program
hx repl              # Start an interactive GHCi session
hx test              # Run tests
```

### Dependencies

```bash
hx lock              # Generate/update hx.lock
hx sync              # Build with locked dependencies
hx add text          # Add a dependency
hx rm text           # Remove a dependency
```

### Code Quality

```bash
hx fmt               # Format code with fourmolu/ormolu
hx fmt --check       # Check formatting without changes
hx lint              # Run hlint
hx lint --fix        # Apply automatic fixes
```

### Toolchain

```bash
hx toolchain status                # Show installed versions
hx toolchain install --ghc 9.8.2   # Install GHC version
hx toolchain use 9.8.2             # Switch GHC version
hx toolchain use project           # Use project's toolchain
```

### Diagnostics

```bash
hx doctor            # Diagnose setup issues
```

## Configuration

hx uses `hx.toml` for project configuration:

```toml
[project]
name = "myapp"
kind = "bin"         # or "lib"
resolver = "cabal"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
hls = "2.9.0.0"

[format]
formatter = "fourmolu"

[lint]
hlint = true
```

## Lockfile

The `hx.lock` file ensures reproducible builds:

```toml
version = 1
created_at = "2026-01-15T00:00:00Z"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"

[plan]
compiler_id = "ghc-9.8.2"
platform = "x86_64-linux"
hash = "sha256:..."

[[packages]]
name = "text"
version = "2.1.1"
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `HX_VERBOSE` | Enable verbose output |
| `HX_QUIET` | Suppress output |
| `HX_NO_COLOR` | Disable colored output |
| `HX_CONFIG_FILE` | Path to config file |
| `HX_CACHE_DIR` | Cache directory location |

## Global Flags

```bash
hx --verbose ...     # Enable verbose output
hx --quiet ...       # Suppress output
hx --no-color ...    # Disable colors
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Usage error |
| 3 | Configuration error |
| 4 | Toolchain error |
| 5 | Build failure |

## Architecture

hx is a Rust workspace with these crates:

| Crate | Purpose |
|-------|---------|
| `hx-cli` | Command-line interface |
| `hx-core` | Shared types and utilities |
| `hx-config` | Configuration parsing |
| `hx-lock` | Lockfile management |
| `hx-toolchain` | GHC/Cabal detection and installation |
| `hx-cabal` | Cabal wrapper |
| `hx-cache` | Build cache management |
| `hx-doctor` | Diagnostic checks |
| `hx-ui` | Terminal output utilities |
| `hx-warnings` | Warning system |

## Development

```bash
# Build
cargo build

# Run tests
cargo test --workspace

# Run clippy
cargo clippy --workspace -- -D warnings

# Format
cargo fmt
```

## Philosophy

hx follows the [Astral](https://astral.sh/) approach:

1. **Wrap first** - Use existing tools (Cabal, GHCup) rather than reimplementing
2. **Tame second** - Add better UX, error messages, and workflows
3. **Replace last** - Only replace components when truly necessary

## License

MIT
