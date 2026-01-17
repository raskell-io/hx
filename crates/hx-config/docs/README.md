# hx-config

Project configuration parsing, detection, and IDE integration for hx.

## Overview

`hx-config` handles:

- Parsing `hx.toml` manifest files
- Project root detection
- Workspace (multi-package) support
- `.cabal` file discovery
- IDE configuration (`hie.yaml` generation)
- HLS version compatibility

## Modules

| Module | Description |
|--------|-------------|
| `manifest` | hx.toml TOML parsing |
| `project` | Project loading and detection |
| `combine` | Configuration merging |
| `ide` | hie.yaml generation |

## Quick Start

```rust
use hx_config::{Project, find_project_root};

// Find project root from current directory
let root = find_project_root(".")?;

// Load project configuration
let project = Project::load(&root)?;

println!("Project: {}", project.name());
println!("GHC version: {:?}", project.manifest.toolchain.ghc);

// Check if it's a workspace
if project.is_workspace() {
    for pkg in project.package_names() {
        println!("  Package: {}", pkg);
    }
}
```

## Documentation

- [hx.toml Reference](./manifest.md) - Complete manifest specification
- [Project Detection](./project.md) - How projects are discovered
- [Workspaces](./workspaces.md) - Multi-package projects
- [IDE Integration](./ide.md) - hie.yaml and HLS setup
