# hx-solver

Dependency resolution and package index management for Haskell.

## Overview

`hx-solver` provides:

- **Version constraint parsing** - PVP-compliant version handling
- **Dependency resolution** - Backtracking solver
- **Hackage index** - Package index loading and caching
- **.cabal parsing** - Extract package metadata
- **Module graphs** - Import dependency analysis
- **Build plans** - Topologically sorted package lists

## Modules

| Module | Description |
|--------|-------------|
| `version` | Version and constraint types |
| `resolver` | Backtracking dependency resolver |
| `index` | Hackage package index |
| `fetch` | Package downloading |
| `cabal` | .cabal file parsing |
| `modules` | Module import graph |
| `plan` | Build plan generation |

## Quick Start

```rust
use hx_solver::{Resolver, PackageIndex, VersionConstraint};

// Load Hackage index
let index = PackageIndex::load_or_update()?;

// Create resolver
let resolver = Resolver::new(&index);

// Resolve dependencies
let plan = resolver.resolve(
    "aeson",
    &VersionConstraint::parse(">=2.0")?,
)?;

for pkg in &plan.packages {
    println!("{} = {}", pkg.name, pkg.version);
}
```

## Version Types

### Version

```rust
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub extra: Vec<u32>,  // Additional components
}

impl Version {
    pub fn parse(s: &str) -> Result<Self>;
}
```

### VersionConstraint

```rust
pub enum VersionConstraint {
    /// Any version
    Any,

    /// Exactly this version
    Exact(Version),

    /// >= version
    AtLeast(Version),

    /// < version
    LessThan(Version),

    /// >= min && < max
    Range { min: Version, max: Version },
}

impl VersionConstraint {
    pub fn parse(s: &str) -> Result<Self>;
    pub fn satisfies(&self, version: &Version) -> bool;
}
```

### Constraint Syntax

| Syntax | Meaning |
|--------|---------|
| `*` | Any version |
| `1.2.3` | Exactly 1.2.3 |
| `>=1.2` | 1.2 or higher |
| `<2.0` | Below 2.0 |
| `>=1.0 && <2.0` | Range |
| `^1.2` | Compatible (>=1.2 && <2) |

## Resolver

```rust
pub struct Resolver<'a> {
    index: &'a PackageIndex,
}

impl<'a> Resolver<'a> {
    pub fn new(index: &'a PackageIndex) -> Self;

    /// Resolve a single package
    pub fn resolve(
        &self,
        package: &str,
        constraint: &VersionConstraint,
    ) -> Result<InstallPlan>;

    /// Resolve multiple packages
    pub fn resolve_all(
        &self,
        packages: &[(&str, &VersionConstraint)],
    ) -> Result<InstallPlan>;
}
```

### InstallPlan

```rust
pub struct InstallPlan {
    /// Resolved packages
    pub packages: Vec<ResolvedPackage>,

    /// Topological build order
    pub order: Vec<String>,
}

pub struct ResolvedPackage {
    pub name: String,
    pub version: Version,
    pub dependencies: Vec<Dependency>,
}
```

## Package Index

### Loading the Index

```rust
use hx_solver::index::{load_index, update_index, default_index_path};

// Load cached index
let index = load_index(&default_index_path())?;

// Update from Hackage
update_index()?;
```

### Package Lookup

```rust
// Find package
let pkg = index.get("aeson")?;

// List versions
for version in pkg.versions.keys() {
    println!("aeson-{}", version);
}

// Get specific version
let aeson_2 = pkg.versions.get(&Version::parse("2.2.1.0")?);
```

### PackageIndex

```rust
pub struct PackageIndex {
    pub packages: HashMap<String, Package>,
}

pub struct Package {
    pub name: String,
    pub versions: HashMap<Version, PackageVersion>,
}

pub struct PackageVersion {
    pub name: String,
    pub version: Version,
    pub dependencies: Vec<Dependency>,
    pub revision: u32,
    pub hash: Option<String>,
}
```

## Cabal Parsing

### Basic Parsing

```rust
use hx_solver::cabal::parse_cabal;

let cabal = parse_cabal(&path)?;
println!("Name: {}", cabal.name);
println!("Version: {}", cabal.version);
```

### Full Parsing

```rust
use hx_solver::cabal::parse_cabal_full;

let cabal = parse_cabal_full(&path)?;

if let Some(lib) = &cabal.library {
    for module in &lib.exposed_modules {
        println!("Module: {}", module);
    }
}
```

### CabalFile

```rust
pub struct CabalFile {
    pub name: String,
    pub version: String,
    pub library: Option<LibraryConfig>,
    pub executables: Vec<ExecutableConfig>,
    pub build_type: BuildType,
    pub build_depends: Vec<Dependency>,
}
```

## Module Graph

```rust
use hx_solver::modules::{build_module_graph, find_haskell_files};

// Find all source files
let files = find_haskell_files(&src_dir)?;

// Build import graph
let graph = build_module_graph(&src_dir)?;

// Get build order
for module in graph.topological_order() {
    println!("Compile: {}", module);
}
```

### ModuleGraph

```rust
pub struct ModuleGraph {
    pub nodes: HashMap<String, ModuleInfo>,
    pub edges: Vec<(String, String)>,
}

pub struct ModuleInfo {
    pub name: String,
    pub file: PathBuf,
    pub imports: Vec<String>,
}
```

## Build Plans

```rust
use hx_solver::plan::generate_build_plan;

let plan = generate_build_plan(&packages, &options)?;

for unit in &plan.units {
    println!("Build: {} ({})", unit.package, unit.package_type);
}
```

## Fetching Packages

```rust
use hx_solver::fetch::{fetch_packages, default_package_cache_dir};

// Download packages
fetch_packages(&["aeson-2.2.1.0", "text-2.1"], &options)?;

// Get cache location
let cache = default_package_cache_dir()?;
```
