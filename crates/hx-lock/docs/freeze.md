# Freeze File Integration

hx can import package versions from Cabal's freeze files.

## Overview

Cabal freeze files (`cabal.project.freeze`) pin exact package versions. hx can parse these to populate the lockfile.

## Freeze File Format

```
-- cabal.project.freeze
constraints: any.aeson ==2.2.1.0,
             any.base ==4.19.0.0,
             any.bytestring ==0.12.0.2,
             any.text ==2.1,
             any.vector ==0.13.1.0
```

## Parsing Freeze Files

```rust
use hx_lock::parse_freeze_file;

let content = std::fs::read_to_string("cabal.project.freeze")?;
let packages = parse_freeze_file(&content)?;

for (name, version) in packages {
    println!("{} == {}", name, version);
}
```

## parse_freeze_file

```rust
/// Parse a Cabal freeze file into package versions.
///
/// Returns a Vec of (package_name, version) tuples.
pub fn parse_freeze_file(content: &str) -> Result<Vec<(String, String)>, LockError>;
```

### Parsing Rules

1. Lines starting with `--` are comments
2. `constraints:` starts the constraint list
3. Each constraint is `any.{name} =={version},`
4. Whitespace and line continuations are handled
5. Trailing comma is optional on last item

## Converting to Lockfile

```rust
use hx_lock::{Lockfile, LockedPackage, parse_freeze_file};

let freeze_content = std::fs::read_to_string("cabal.project.freeze")?;
let packages = parse_freeze_file(&freeze_content)?;

let mut lockfile = Lockfile::new();

for (name, version) in packages {
    lockfile.add_package(LockedPackage {
        name,
        version,
        source: "hackage".into(),
        hash: None,
    });
}

lockfile.to_file("hx.lock")?;
```

## CLI Usage

```bash
# Generate lockfile from freeze
hx lock --from-freeze cabal.project.freeze

# Generate freeze from lockfile
hx lock --to-freeze
```

## Freeze vs Lockfile

| Aspect | cabal.project.freeze | hx.lock |
|--------|---------------------|---------|
| Format | Cabal constraints | TOML |
| Toolchain | Not tracked | GHC/Cabal versions |
| Workspace | Not tracked | Package list |
| Hashes | Not included | Optional per-package |
| Fingerprint | Not included | Computed |

## Workflow Integration

### Migrating from Freeze

```bash
# 1. Generate freeze with Cabal
cabal freeze

# 2. Import to hx.lock
hx lock --from-freeze

# 3. Remove freeze file (optional)
rm cabal.project.freeze
```

### Exporting for Cabal

```bash
# Generate freeze from lockfile
hx lock --to-freeze

# Use with cabal
cabal build --freeze-file=cabal.project.freeze
```

## Limitations

The freeze parser handles common cases:

- ✅ Standard `any.{pkg} =={version}` format
- ✅ Multi-line continuations
- ✅ Comments
- ⚠️ Flag constraints (parsed but ignored)
- ❌ Source constraints (not supported)

Example of ignored constraints:

```
constraints: any.aeson ==2.2.1.0,
             any.aeson +ordered-keymap,    -- flag (ignored)
             source.custom-pkg git://...   -- source (error)
```
