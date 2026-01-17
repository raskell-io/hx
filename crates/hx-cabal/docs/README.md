# hx-cabal

Cabal wrapper, build orchestration, and output parsing for hx.

## Overview

`hx-cabal` provides:

- **Cabal invocation** - Wrapper for cabal build/test/run
- **Output parsing** - Extract errors, warnings from GHC output
- **Native builds** - Direct GHC compilation (experimental)
- **Diagnostics** - JSON diagnostic parsing for LSP

## Modules

| Module | Description |
|--------|-------------|
| `build` | Cabal build/test/run commands |
| `native` | Direct GHC native builder |
| `full_native` | Complete native build pipeline |
| `fetch` | Parallel package downloading |
| `freeze` | Freeze file generation |
| `ghc_diagnostics` | GHC diagnostic parsing |
| `edit` | .cabal file editing |

## Quick Start

```rust
use hx_cabal::build::{build, BuildOptions};
use hx_ui::Output;

let options = BuildOptions {
    release: true,
    jobs: Some(4),
    target: None,
    package: None,
    verbose: false,
    ..Default::default()
};

let output = Output::new();
build(&project_root, &build_dir, &options, &output).await?;
```

## Documentation

- [Building](./building.md) - Build, test, and run commands
- [Native Builds](./native.md) - Experimental native GHC builder
- [Diagnostics](./diagnostics.md) - Error parsing for IDE integration
- [Package Management](./packages.md) - Dependency fetching and freezing
