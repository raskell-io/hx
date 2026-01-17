# Workspaces

hx supports multi-package workspaces (monorepos) with shared configuration.

## Overview

A workspace contains multiple Haskell packages that share:
- Toolchain configuration
- Build settings
- Dependency constraints
- Plugin hooks

## Workspace Structure

```
my-workspace/
├── hx.toml                 # Root manifest
├── hx.lock                 # Shared lockfile
├── cabal.project           # Cabal project file
├── packages/
│   ├── core/
│   │   ├── core.cabal
│   │   └── src/
│   ├── server/
│   │   ├── server.cabal
│   │   └── src/
│   └── client/
│       ├── client.cabal
│       └── src/
```

## Workspace Detection

A project is a workspace when:
1. `cabal.project` exists, OR
2. Multiple `.cabal` files are found

```rust
let project = Project::load(&root)?;

if project.is_workspace() {
    println!("Workspace with {} packages", project.package_count());
}
```

## WorkspacePackage

Each package in a workspace is represented by:

```rust
pub struct WorkspacePackage {
    /// Package name
    pub name: String,

    /// Package directory
    pub path: PathBuf,

    /// Path to .cabal file
    pub cabal_file: PathBuf,
}
```

## Accessing Packages

```rust
let project = Project::load(&root)?;

// List all packages
for name in project.package_names() {
    println!("Package: {}", name);
}

// Get specific package
if let Some(pkg) = project.get_package("server") {
    println!("Server path: {}", pkg.path.display());
}

// Iterate packages
for pkg in &project.workspace_packages {
    println!("{}: {}", pkg.name, pkg.cabal_file.display());
}
```

## Building Workspaces

### Build All Packages

```bash
hx build
```

### Build Specific Package

```bash
hx build --package server
```

### Build Multiple Packages

```bash
hx build --package core --package server
```

## Testing Workspaces

```bash
# Test all packages
hx test

# Test specific package
hx test --package core
```

## cabal.project Integration

hx reads `cabal.project` to discover packages:

```
-- cabal.project
packages:
    packages/core
    packages/server
    packages/client

-- Optional: shared constraints
constraints: text >= 2.0
```

## Workspace Manifest

The root `hx.toml` applies to all packages:

```toml
[project]
name = "my-workspace"
kind = "lib"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"

[build]
optimization = 1
warnings = true

[dependencies]
# Shared dependencies
base = ">=4.18"
text = "^2.0"
```

## Package-Specific Configuration

Individual packages can have their own `hx.toml`:

```
packages/server/
├── hx.toml          # Package-specific overrides
├── server.cabal
└── src/
```

Package configuration merges with workspace configuration:
- Package values override workspace values
- Dependencies are combined

## Status Display

Build output shows workspace context:

```
   Building my-workspace (3/3 packages)
   Compiling core-0.1.0
   Compiling server-0.1.0
   Compiling client-0.1.0
    Finished 3 packages in 12.5s
```

## Lockfile for Workspaces

The lockfile records all packages:

```toml
[workspace]
is_workspace = true

[[workspace.packages]]
name = "core"
version = "0.1.0"
path = "packages/core"

[[workspace.packages]]
name = "server"
version = "0.1.0"
path = "packages/server"
```

## Best Practices

1. **Use cabal.project** - Standard Cabal workspace format
2. **Share toolchain config** - One GHC version for all
3. **Centralize dependencies** - Define in root hx.toml
4. **Test together** - `hx test` runs all package tests
5. **Lock together** - Single hx.lock for reproducibility
