# Dependency Resolution

`hx-solver` uses a backtracking constraint satisfaction algorithm to resolve dependencies.

## Overview

The resolver finds a set of package versions that:
1. Satisfy all version constraints
2. Include all transitive dependencies
3. Have no conflicting requirements

## Basic Usage

```rust
use hx_solver::{Resolver, PackageIndex, VersionConstraint, load_index};

// Load the Hackage index
let index = load_index(&index_path, &options)?;

// Create resolver
let resolver = Resolver::new(&index);

// Resolve a single package
let plan = resolver.resolve("aeson", &VersionConstraint::Any)?;

for pkg in &plan.packages {
    println!("{} {}", pkg.name, pkg.version);
}
```

## Resolving Multiple Dependencies

```rust
use hx_solver::Dependency;

let deps = vec![
    Dependency::with_constraint("aeson", parse_constraint(">= 2.0")?),
    Dependency::with_constraint("text", parse_constraint(">= 2.0")?),
    Dependency::with_constraint("bytestring", VersionConstraint::Any),
];

let plan = resolver.resolve_all(&deps)?;
```

## Configuration

Customize resolver behavior with `ResolverConfig`:

```rust
use hx_solver::ResolverConfig;

let config = ResolverConfig {
    // Maximum backtracking attempts before giving up
    max_attempts: 100_000,

    // Prefer newer versions (true) or older (false)
    prefer_newest: true,

    // Pre-installed packages (won't be resolved)
    installed: HashMap::from([
        ("base".to_string(), "4.18.0".parse().unwrap()),
        ("ghc-prim".to_string(), "0.10.0".parse().unwrap()),
    ]),
};

let resolver = Resolver::with_config(&index, config);
```

## Resolution Algorithm

### 1. Initialize

```
pending = [root requirements]
selected = {}
choices = []
```

### 2. Process Requirements

For each pending requirement:

```
while pending is not empty:
    req = pending.pop()

    if req.package already selected:
        if selected version satisfies constraint:
            continue
        else:
            backtrack or fail

    candidates = find matching versions
    if no candidates:
        backtrack or fail

    select best candidate (newest by default)
    save choice point with alternatives
    add dependencies of selected version to pending
```

### 3. Backtracking

When a conflict is found:

```
backtrack():
    while choices not empty:
        choice = choices.pop()
        undo selection

        if alternatives remain:
            try next alternative
            return success

    return failure (no solution)
```

## Install Plan

The result is an `InstallPlan` with packages in dependency order:

```rust
pub struct InstallPlan {
    pub packages: Vec<ResolvedPackage>,
}

pub struct ResolvedPackage {
    pub name: String,
    pub version: Version,
    pub dependencies: Vec<String>,
}

// Query the plan
let version = plan.get_version("aeson");
let build_order = plan.build_order();
```

## Error Types

### PackageNotFound

Package doesn't exist in the index:

```rust
ResolveError::PackageNotFound { name: "nonexistent".to_string() }
```

**Fix**: Check package name spelling, run `cabal update`.

### NoMatchingVersion

No version satisfies the constraint:

```rust
ResolveError::NoMatchingVersion {
    package: "aeson".to_string(),
    constraint: parse_constraint(">= 99.0").unwrap(),
    available: vec!["2.2.1.0", "2.1.0", "2.0.0"],
}
```

**Fix**: Relax the version constraint.

### VersionConflict

Selected version doesn't satisfy a new constraint:

```rust
ResolveError::VersionConflict {
    package: "text".to_string(),
    required: parse_constraint(">= 2.1").unwrap(),
    selected: "2.0.0".parse().unwrap(),
    required_by: vec!["aeson".to_string()],
}
```

**Fix**: Check for conflicting bounds, widen version ranges.

### CycleDetected

Dependency graph has a cycle:

```rust
ResolveError::CycleDetected(vec!["A", "B", "C", "A"])
```

**Fix**: Report to package maintainers - this is a package metadata bug.

### Exhausted

Solver gave up after too many attempts:

```rust
ResolveError::Exhausted { attempts: 100000 }
```

**Fix**: Simplify dependencies, pin specific versions.

## Performance

### Version Ordering

By default, the resolver tries newest versions first (`prefer_newest: true`). This often finds solutions faster since most projects want recent versions.

### Backtracking Limit

The `max_attempts` limit (default: 100,000) prevents infinite loops on unsolvable problems. Increase for very large dependency trees.

### Pre-installed Packages

Mark GHC boot packages as `installed` to skip resolution:

```rust
let config = ResolverConfig {
    installed: HashMap::from([
        ("base".to_string(), "4.18.0".parse().unwrap()),
        ("ghc-prim".to_string(), "0.10.0".parse().unwrap()),
        ("template-haskell".to_string(), "2.20.0".parse().unwrap()),
    ]),
    ..Default::default()
};
```

## Caching

Resolution results can be cached:

```rust
use hx_solver::{save_resolution_cache, load_cached_resolution, compute_deps_fingerprint};

// Compute a fingerprint of the dependencies
let fingerprint = compute_deps_fingerprint(&deps)?;

// Try to load cached resolution
if let Some(cached) = load_cached_resolution(&fingerprint)? {
    return Ok(cached);
}

// Resolve and cache
let plan = resolver.resolve_all(&deps)?;
save_resolution_cache(&fingerprint, &plan)?;
```

## Example: Full Resolution Flow

```rust
use hx_solver::{
    Resolver, ResolverConfig, PackageIndex,
    Dependency, VersionConstraint, parse_constraint,
    load_index, default_index_path,
};
use std::collections::HashMap;

fn resolve_project_deps() -> Result<InstallPlan> {
    // Load index
    let index_path = default_index_path()?;
    let index = load_index(&index_path, &IndexOptions::default())?;

    // Configure resolver with GHC boot packages
    let config = ResolverConfig {
        max_attempts: 100_000,
        prefer_newest: true,
        installed: ghc_boot_packages("9.8.2"),
    };

    let resolver = Resolver::with_config(&index, config);

    // Define project dependencies
    let deps = vec![
        Dependency::with_constraint("aeson", parse_constraint("^>= 2.2")?),
        Dependency::with_constraint("servant", parse_constraint(">= 0.19")?),
        Dependency::with_constraint("warp", parse_constraint(">= 3.3")?),
    ];

    // Resolve
    let plan = resolver.resolve_all(&deps)?;

    // Print build order
    println!("Build plan ({} packages):", plan.packages.len());
    for pkg in &plan.packages {
        println!("  {} {}", pkg.name, pkg.version);
    }

    Ok(plan)
}

fn ghc_boot_packages(ghc_version: &str) -> HashMap<String, Version> {
    // These come with GHC and don't need resolution
    HashMap::from([
        ("base".to_string(), "4.18.0".parse().unwrap()),
        ("ghc-prim".to_string(), "0.10.0".parse().unwrap()),
        ("integer-gmp".to_string(), "1.1".parse().unwrap()),
        // ... more boot packages
    ])
}
```

## Debugging Resolution

Enable tracing for detailed output:

```bash
RUST_LOG=hx_solver::resolver=trace hx build
```

This shows:
- Each requirement being processed
- Version candidates considered
- Selection decisions
- Backtracking events
