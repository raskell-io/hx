# Build State

`BuildState` tracks build history for incremental compilation and status reporting.

## Overview

```rust
use hx_cache::BuildState;

let state = BuildState::load(&project_root)?;

// Check if build is fresh
if state.is_fresh(&source_hash, Some(&lock_hash)) {
    println!("No rebuild needed");
}
```

## BuildState

```rust
pub struct BuildState {
    /// Last successful source fingerprint
    pub last_source_fingerprint: Option<String>,

    /// Last successful lockfile fingerprint
    pub last_lock_fingerprint: Option<String>,

    /// GHC version used for last build
    pub ghc_version: Option<String>,

    /// Per-package build info
    pub packages: HashMap<String, PackageBuildInfo>,

    /// Last build timestamp
    pub last_build_at: Option<u64>,

    /// Last build duration (seconds)
    pub last_build_duration: Option<f64>,

    /// Last failure info
    pub last_failure: Option<BuildFailure>,
}
```

## PackageBuildInfo

```rust
pub struct PackageBuildInfo {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Content fingerprint at build time
    pub fingerprint: String,

    /// Build timestamp
    pub built_at: u64,
}
```

## Loading and Saving

```rust
// Load (returns default if not found)
let state = BuildState::load(&project_root)?;

// Save
state.save(&project_root)?;
```

State is stored in `project/.hx/build-state.json`.

## Checking Freshness

```rust
let state = BuildState::load(&project_root)?;

// Check if build can be skipped
let is_fresh = state.is_fresh(
    &current_source_fingerprint,
    lockfile_fingerprint.as_deref(),
);

if is_fresh {
    output.status("Fresh", project_name);
    return Ok(0);
}
```

### Freshness Criteria

A build is fresh when ALL of these match:
1. Source fingerprint unchanged
2. Lockfile fingerprint unchanged
3. No previous build failure

## Updating State

### After Successful Build

```rust
let build_duration = start.elapsed();

state.update_fingerprints(
    &source_fingerprint,
    lockfile_fingerprint.as_deref(),
    ghc_version.as_deref(),
);

state.mark_success(
    package_name,
    &source_fingerprint,
    build_duration.as_secs_f64(),
);

state.save(&project_root)?;
```

### After Failed Build

```rust
state.mark_failed(package_name, &error_message);
state.save(&project_root)?;
```

## Workspace Support

Track each package independently:

```rust
for package in &workspace.packages {
    let pkg_fingerprint = compute_package_fingerprint(package)?;

    if let Some(info) = state.packages.get(&package.name) {
        if info.fingerprint == pkg_fingerprint {
            println!("{} is up-to-date", package.name);
            continue;
        }
    }

    // Rebuild package...
    state.packages.insert(package.name.clone(), PackageBuildInfo {
        name: package.name.clone(),
        version: package.version.clone(),
        fingerprint: pkg_fingerprint,
        built_at: now(),
    });
}
```

## Build History

```rust
// Last build info
if let Some(ts) = state.last_build_at {
    println!("Last built: {}", format_timestamp(ts));
}

if let Some(duration) = state.last_build_duration {
    println!("Build took: {:.2}s", duration);
}

// Check for previous failures
if let Some(failure) = &state.last_failure {
    println!("Last failure: {} in {}", failure.error, failure.package);
}
```

## Clearing State

```rust
// Reset build state (force rebuild)
let state = BuildState::default();
state.save(&project_root)?;
```

Or delete the file:

```bash
rm project/.hx/build-state.json
```

## JSON Format

```json
{
  "last_source_fingerprint": "sha256:abc123...",
  "last_lock_fingerprint": "sha256:def456...",
  "ghc_version": "9.8.2",
  "last_build_at": 1705312800,
  "last_build_duration": 12.5,
  "packages": {
    "my-lib": {
      "name": "my-lib",
      "version": "0.1.0",
      "fingerprint": "sha256:...",
      "built_at": 1705312800
    }
  }
}
```
