# Fingerprinting

Source fingerprinting enables accurate change detection for incremental builds.

## Overview

A fingerprint is a hash of all source files that can affect the build:

```rust
use hx_cache::compute_source_fingerprint;

let fingerprint = compute_source_fingerprint(&project_root)?;
println!("Hash: {}", fingerprint.hash);
println!("Files: {}", fingerprint.files_count);
```

## SourceFingerprint

```rust
pub struct SourceFingerprint {
    /// SHA256 hash of all source content
    pub hash: String,

    /// Number of files included
    pub files_count: usize,

    /// Unix timestamp when computed
    pub computed_at: u64,
}
```

## What's Included

The fingerprint includes:

| File Type | Pattern |
|-----------|---------|
| Haskell sources | `**/*.hs`, `**/*.lhs` |
| Cabal files | `*.cabal` |
| Project config | `hx.toml` |
| Cabal project | `cabal.project`, `cabal.project.local` |

## Fingerprint Algorithm

```rust
impl SourceFingerprint {
    pub fn compute(root: &Path) -> Result<Self> {
        let mut hasher = Sha256::new();
        let mut files_count = 0;

        // Collect and sort files for determinism
        let mut files: Vec<PathBuf> = find_source_files(root)?;
        files.sort();

        for file in &files {
            // Hash file path (relative)
            let relative = file.strip_prefix(root)?;
            hasher.update(relative.to_string_lossy().as_bytes());

            // Hash file content
            let content = std::fs::read(file)?;
            hasher.update(&content);

            files_count += 1;
        }

        Ok(SourceFingerprint {
            hash: format!("sha256:{:x}", hasher.finalize()),
            files_count,
            computed_at: now_unix_timestamp(),
        })
    }
}
```

## Saving and Loading

```rust
use hx_cache::{compute_source_fingerprint, save_source_fingerprint, load_source_fingerprint};

// Compute and save
let fp = compute_source_fingerprint(&project_root)?;
save_source_fingerprint(&project_root, &fp)?;

// Load previously saved
let saved = load_source_fingerprint(&project_root)?;
```

Fingerprints are saved to `project/.hx/fingerprint`.

## Comparing Fingerprints

```rust
let current = compute_source_fingerprint(&project_root)?;
let saved = load_source_fingerprint(&project_root)?;

if current.hash == saved.hash {
    println!("No source changes detected");
} else {
    println!("Sources changed, rebuild needed");
}
```

## Build State Integration

```rust
use hx_cache::BuildState;

let fingerprint = compute_source_fingerprint(&project_root)?;
let build_state = BuildState::load(&project_root)?;

// Check if build is fresh
if build_state.is_fresh(&fingerprint.hash, lockfile_hash.as_deref()) {
    println!("Build is up-to-date");
    return Ok(());
}

// After successful build, update state
build_state.update_fingerprints(
    &fingerprint.hash,
    lockfile_hash.as_deref(),
    ghc_version.as_deref(),
);
build_state.save(&project_root)?;
```

## Determinism

Fingerprints are deterministic:

1. **Sorted file list** - Files processed in consistent order
2. **Relative paths** - Same project = same paths
3. **Content only** - Timestamps don't affect hash
4. **Cross-platform** - Normalized path separators

## Exclusions

These are NOT included in fingerprints:

- `.hx/` directory
- `dist-newstyle/` (Cabal build output)
- `.git/` directory
- Binary files
- Generated files

## Performance

Fingerprinting is fast:

- Only reads source files
- Uses streaming hash (low memory)
- Skips excluded directories early
- Typical: <100ms for medium projects

## Caching Strategy

```
┌─────────────────────────────────────────┐
│ Source files changed?                    │
│ (compare fingerprint)                    │
└─────────────────────────────────────────┘
              │
     ┌────────┴────────┐
     │ No              │ Yes
     ▼                 ▼
┌─────────┐     ┌─────────────┐
│ Skip    │     │ Lockfile    │
│ build   │     │ changed?    │
└─────────┘     └─────────────┘
                       │
              ┌────────┴────────┐
              │ No              │ Yes
              ▼                 ▼
        ┌───────────┐    ┌───────────────┐
        │ Rebuild   │    │ Full rebuild  │
        │ sources   │    │ (deps + src)  │
        └───────────┘    └───────────────┘
```
