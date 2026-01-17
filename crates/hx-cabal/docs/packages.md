# Package Management

`hx-cabal` provides utilities for dependency fetching and .cabal file editing.

## Fetching Packages

Download dependencies in parallel:

```rust
use hx_cabal::fetch::{fetch_packages, FetchOptions};

let options = FetchOptions {
    jobs: Some(8),
    verbose: false,
};

let result = fetch_packages(&project_root, &options).await?;
println!("Fetched {} packages", result.packages_fetched);
```

## FetchOptions

```rust
pub struct FetchOptions {
    /// Number of parallel download jobs
    pub jobs: Option<usize>,

    /// Enable verbose output
    pub verbose: bool,
}
```

## FetchResult

```rust
pub struct FetchResult {
    /// Fetch succeeded
    pub success: bool,

    /// Number of packages fetched
    pub packages_fetched: usize,

    /// Total duration
    pub duration: Duration,
}
```

## Freeze Files

Generate Cabal freeze files:

```rust
use hx_cabal::freeze::generate_freeze;

let freeze_content = generate_freeze(&project_root).await?;
std::fs::write("cabal.project.freeze", freeze_content)?;
```

## Editing .cabal Files

### Adding Dependencies

```rust
use hx_cabal::edit::add_dependency;

add_dependency(
    &cabal_file_path,
    "aeson",           // Package name
    Some(">=2.0"),     // Version constraint
    "library",         // Section (library, executable, test-suite)
)?;
```

Before:
```cabal
library
  build-depends:
    base >= 4.18
```

After:
```cabal
library
  build-depends:
    base >= 4.18,
    aeson >= 2.0
```

### Removing Dependencies

```rust
use hx_cabal::edit::remove_dependency;

remove_dependency(
    &cabal_file_path,
    "old-package",
    "library",
)?;
```

## Package Info

Query installed package information:

```rust
use hx_cabal::native::query_package_info;

let info = query_package_info("aeson")?;
println!("Version: {}", info.version);
println!("Dependencies: {:?}", info.dependencies);
```

## PackageInfo

```rust
pub struct PackageInfo {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Direct dependencies
    pub dependencies: Vec<String>,
}
```

## Transitive Dependencies

Resolve the full dependency closure:

```rust
use hx_cabal::native::resolve_transitive_deps;

let all_deps = resolve_transitive_deps(&["aeson", "text"])?;
// ["aeson", "text", "bytestring", "containers", "base", ...]
```

## Pre-Installed Packages

Check if a package comes with GHC:

```rust
use hx_cabal::full_native::{is_pre_installed, pre_installed_packages};

if is_pre_installed("base") {
    println!("base is bundled with GHC");
}

// List all pre-installed packages
for pkg in pre_installed_packages() {
    println!("{}", pkg);
}
```

### Common Pre-Installed Packages

- `base`
- `ghc-prim`
- `integer-gmp` / `integer-simple`
- `bytestring`
- `containers`
- `deepseq`
- `directory`
- `filepath`
- `process`
- `template-haskell`
- `text`
- `time`
- `transformers`
- `array`

## Package Database

```rust
use hx_cabal::native::GhcConfig;

let config = GhcConfig::detect().await?;

// Global package DB
// e.g., /usr/lib/ghc-9.8.2/package.conf.d

// User package DB
// e.g., ~/.ghc/x86_64-darwin-9.8.2/package.conf.d

for db in &config.package_dbs {
    println!("Package DB: {}", db.display());
}
```

## Error Handling

```rust
use hx_cabal::edit::{add_dependency, CabalEditError};

match add_dependency(&path, "new-dep", None, "library") {
    Ok(()) => println!("Dependency added"),
    Err(CabalEditError::FileNotFound) => {
        eprintln!("No .cabal file found");
    }
    Err(CabalEditError::ParseError(msg)) => {
        eprintln!("Failed to parse .cabal: {}", msg);
    }
    Err(CabalEditError::SectionNotFound(section)) => {
        eprintln!("Section not found: {}", section);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```
