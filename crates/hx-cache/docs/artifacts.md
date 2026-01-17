# Artifacts

Content-addressed storage for compiled Haskell artifacts.

## Overview

Artifacts (`.o`, `.hi` files) are cached by content hash:

```rust
use hx_cache::{store_artifacts, retrieve_artifacts, compute_artifact_hash};

// Store compiled artifacts
store_artifacts(&artifacts_dir, &fingerprint)?;

// Retrieve if available
if let Some(cached) = retrieve_artifacts(&fingerprint)? {
    copy_artifacts(&cached, &build_dir)?;
}
```

## StoreIndex

```rust
pub struct StoreIndex {
    /// Format version
    pub version: u32,

    /// Cached build entries keyed by fingerprint
    pub entries: HashMap<String, BuildCacheEntry>,
}

pub struct BuildCacheEntry {
    /// Build fingerprint
    pub fingerprint: String,

    /// When the build was cached
    pub built_at: u64,

    /// Last access time (for pruning)
    pub last_accessed: u64,

    /// GHC version used
    pub ghc_version: Option<String>,

    /// Target platform
    pub platform: String,

    /// Number of packages
    pub package_count: usize,

    /// Project name
    pub project_name: Option<String>,
}
```

## Store Operations

```rust
use hx_cache::StoreIndex;

// Load index
let mut index = StoreIndex::load()?;

// Check for cache hit
if index.has_cached_build(&fingerprint) {
    // Use cached build
}

// Record new build
index.record_build(BuildCacheEntry {
    fingerprint: fingerprint.clone(),
    built_at: now(),
    last_accessed: now(),
    ghc_version: Some("9.8.2".into()),
    platform: "aarch64-apple-darwin".into(),
    package_count: 5,
    project_name: Some("my-project".into()),
});

index.save()?;
```

## Artifact Storage

```
~/.cache/hx/store/
├── index.json
├── sha256_abc123/
│   ├── artifacts/
│   │   ├── Main.o
│   │   ├── Main.hi
│   │   ├── Lib.o
│   │   └── Lib.hi
│   └── metadata.json
└── sha256_def456/
    └── ...
```

## Content Addressing

Artifacts are stored by content hash for deduplication:

```rust
use hx_cache::compute_artifact_hash;

let hash = compute_artifact_hash(&artifact_path)?;
// "sha256:789abc..."

// Same content = same hash = single storage
```

## ArtifactIndex

Per-fingerprint artifact tracking:

```rust
pub struct ArtifactIndex {
    /// Artifacts keyed by relative path
    pub artifacts: HashMap<String, ArtifactEntry>,
}

pub struct ArtifactEntry {
    /// Content hash
    pub hash: String,

    /// Storage path
    pub path: PathBuf,

    /// File size in bytes
    pub size: u64,
}
```

## Storing Artifacts

```rust
use hx_cache::store_artifacts;

// After successful build, cache artifacts
store_artifacts(
    &project_root.join(".hx/build"),
    &fingerprint,
)?;
```

This:
1. Computes content hash for each artifact
2. Copies to store (if not already present)
3. Updates the store index

## Retrieving Artifacts

```rust
use hx_cache::retrieve_artifacts;

if let Some(cache_dir) = retrieve_artifacts(&fingerprint)? {
    // Copy from cache to build directory
    for entry in std::fs::read_dir(&cache_dir)? {
        let entry = entry?;
        std::fs::copy(
            entry.path(),
            build_dir.join(entry.file_name()),
        )?;
    }
}
```

## Pruning

Remove old/unused artifacts:

```rust
use hx_cache::{prune_artifacts, PruneResult};

// Remove artifacts not accessed in 30 days
let result = prune_artifacts(Duration::from_secs(30 * 24 * 60 * 60))?;

println!("Removed {} entries", result.removed_count);
println!("Freed {} bytes", result.freed_bytes);
```

## Stats

```rust
use hx_cache::{StoreIndex, ArtifactStats};

let index = StoreIndex::load()?;
let stats = index.stats();

println!("Cached builds: {}", stats.total_builds);
println!("Total size: {} MB", stats.total_size / 1024 / 1024);
```

## CLI Commands

```bash
# Show cache stats
hx cache stats

# Clean all caches
hx cache clean

# Prune old entries
hx cache prune --older-than 30d
```
