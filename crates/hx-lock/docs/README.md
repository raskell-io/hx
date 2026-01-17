# hx-lock

Lockfile management for deterministic Haskell builds.

## Overview

`hx-lock` handles the `hx.lock` lockfile, which ensures reproducible builds by:

- Recording exact package versions
- Tracking toolchain versions
- Computing content fingerprints
- Supporting workspace packages

## Modules

| Module | Description |
|--------|-------------|
| `lockfile` | Core lockfile types and operations |
| `package` | Package version records |
| `toolchain` | Toolchain version tracking |
| `fingerprint` | Content hashing for cache invalidation |

## Quick Start

```rust
use hx_lock::Lockfile;

// Load existing lockfile
let lockfile = Lockfile::from_file("hx.lock")?;

// Check fingerprint
println!("Lockfile hash: {:?}", lockfile.plan.hash);

// List locked packages
for pkg in &lockfile.packages {
    println!("{} = {}", pkg.name, pkg.version);
}

// Save changes
lockfile.to_file("hx.lock")?;
```

## Documentation

- [Lockfile Format](./format.md) - Complete file specification
- [Fingerprinting](./fingerprint.md) - Cache invalidation strategy
- [Freeze Integration](./freeze.md) - Cabal freeze file parsing
