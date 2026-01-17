# Native Builds

hx includes an experimental native GHC builder that bypasses Cabal for faster builds.

> **Warning**: Native build is experimental. Use `--native` only for testing.

## Overview

The native builder:
- Directly invokes GHC (no Cabal overhead)
- Manages module dependencies
- Supports incremental compilation
- Handles package database configuration

## Enabling Native Build

```bash
hx build --native
```

Or in code:

```rust
use hx_cabal::native::{NativeBuilder, NativeBuildOptions, GhcConfig};

let ghc_config = GhcConfig::detect().await?;
let builder = NativeBuilder::new(ghc_config);

let options = NativeBuildOptions {
    src_dirs: vec![PathBuf::from("src")],
    output_dir: project_root.join(".hx/native-build"),
    optimization: 2,
    warnings: true,
    werror: false,
    extra_flags: vec!["-XOverloadedStrings".into()],
    jobs: num_cpus::get(),
    verbose: false,
    main_module: Some("Main".into()),
    output_exe: Some(project_root.join(".hx/native-build/my-app")),
    output_lib: None,
    native_linking: true,
};

let result = builder.build(&project_root, &options, &output).await?;
```

## GhcConfig

```rust
pub struct GhcConfig {
    /// Path to GHC executable
    pub ghc_path: PathBuf,

    /// GHC version string
    pub version: String,

    /// Package database paths
    pub package_dbs: Vec<PathBuf>,

    /// Available packages
    pub packages: Vec<String>,

    /// Resolved package info
    pub resolved_packages: Vec<PackageInfo>,
}

impl GhcConfig {
    /// Auto-detect GHC configuration
    pub async fn detect() -> Result<Self>;

    /// Add packages from lockfile
    pub fn with_packages(self, packages: Vec<String>) -> Self;
}
```

## NativeBuildOptions

```rust
pub struct NativeBuildOptions {
    /// Source directories to compile
    pub src_dirs: Vec<PathBuf>,

    /// Output directory for artifacts
    pub output_dir: PathBuf,

    /// Optimization level (0, 1, 2)
    pub optimization: u8,

    /// Enable warnings
    pub warnings: bool,

    /// Treat warnings as errors
    pub werror: bool,

    /// Extra GHC flags
    pub extra_flags: Vec<String>,

    /// Parallel compilation jobs
    pub jobs: usize,

    /// Verbose output
    pub verbose: bool,

    /// Main module for executable
    pub main_module: Option<String>,

    /// Output executable path
    pub output_exe: Option<PathBuf>,

    /// Output library path
    pub output_lib: Option<PathBuf>,

    /// Use native linking with resolved packages
    pub native_linking: bool,
}
```

## NativeBuildResult

```rust
pub struct NativeBuildResult {
    /// Build succeeded
    pub success: bool,

    /// Build duration
    pub duration: Duration,

    /// Modules compiled
    pub modules_compiled: usize,

    /// Modules skipped (up-to-date)
    pub modules_skipped: usize,

    /// Warning messages
    pub warnings: Vec<String>,

    /// Error messages
    pub errors: Vec<String>,

    /// Path to executable (if built)
    pub executable: Option<PathBuf>,
}
```

## Build Process

### 1. Module Discovery

Find all `.hs` files in source directories:

```rust
let modules = find_haskell_files(&src_dirs)?;
// ["src/Main.hs", "src/Lib.hs", "src/Utils.hs"]
```

### 2. Dependency Analysis

Build module import graph:

```rust
let graph = build_module_graph(&modules)?;
// Main imports Lib, Lib imports Utils
```

### 3. Topological Sort

Order modules for compilation:

```rust
let order = topological_sort(&graph)?;
// [Utils, Lib, Main]
```

### 4. Incremental Compilation

Check if modules need recompilation:

```rust
for module in &order {
    if needs_recompile(module, &fingerprints)? {
        compile_module(module)?;
    } else {
        skip_module(module);
    }
}
```

### 5. Linking

Link object files into executable:

```rust
if let Some(exe_path) = &options.output_exe {
    link_executable(&object_files, exe_path)?;
}
```

## GHC Flags

The native builder constructs GHC invocations:

```bash
ghc -O2 \
    -Wall \
    -XOverloadedStrings \
    -outputdir .hx/native-build \
    -package-db ~/.ghc/x86_64-darwin-9.8.2/package.conf.d \
    -c src/Lib.hs
```

## Package Database

Packages are resolved from:

1. GHC's global package DB
2. User package DB
3. Lockfile packages (via Cabal store)

```rust
let packages = packages_from_lockfile(&lockfile_path);
let ghc_config = ghc_config.with_packages(packages);
```

## Advantages

- **Faster incremental builds** - Only recompiles changed modules
- **Less overhead** - No Cabal solver/setup
- **Direct control** - Explicit GHC flags

## Limitations

- **No Cabal features** - No Setup.hs, no custom builds
- **Manual dependencies** - Must resolve packages manually
- **Experimental** - May have edge cases

## Output

```
   Building my-app (native)
     GHC version: 9.8.2
     Optimization: -O2
     Parallelism: 8 jobs
   Compiling Utils (1/3)
   Compiling Lib (2/3)
   Compiling Main (3/3)
   Linking my-app
    Finished 3 modules in 1.2s
   Executable: .hx/native-build/my-app
```
