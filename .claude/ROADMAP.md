# HX Roadmap

## v0.1.0 - Initial Release

### Done Definition

hx v0.1.0 is shippable when:
- [x] `hx init` creates a project that builds on at least Linux/macOS
- [x] `hx toolchain status` works robustly
- [x] `hx build/test/run/repl` work for a basic project
- [x] `hx lock` produces deterministic lockfile
- [x] `hx sync` enforces the lock
- [x] `hx doctor` gives actionable fixes for missing tools/mismatch

---

## Milestone A: Scaffolding ✅

**Goal:** Basic project structure and CLI framework

### Tasks
- [x] Create Rust workspace with all crates:
  - `hx-cli`
  - `hx-core`
  - `hx-config`
  - `hx-lock`
  - `hx-toolchain`
  - `hx-cabal`
  - `hx-cache`
  - `hx-doctor`
  - `hx-ui`
  - `hx-telemetry`
  - `hx-warnings`
- [x] Implement `hx --version`
- [x] Implement `hx help`
- [x] Implement config loading (hx.toml parsing)
- [x] Implement project root detection
- [x] Add structured logging with `tracing`
- [x] Add `hx-ui` output helpers (spinners, progress)

### Deliverables
- [x] Workspace compiles
- [x] `cargo run -p hx-cli -- --version` works
- [x] `cargo run -p hx-cli -- help` shows command list

---

## Milestone B: Toolchain Detection ✅

**Goal:** Detect and report installed Haskell tools

### Tasks
- [x] Implement `hx toolchain status`
- [x] Detect `ghc` on PATH, parse version
- [x] Detect `cabal` on PATH, parse version
- [x] Detect `ghcup` on PATH, parse version
- [x] Detect `hls` (haskell-language-server-wrapper), parse version
- [x] Implement basic `hx doctor`:
  - [x] Missing tool detection
  - [x] Version mismatch warnings
  - [x] Actionable fix suggestions

### Deliverables
- [x] `hx toolchain status` shows all detected tools
- [x] `hx doctor` identifies missing/mismatched tools

---

## Milestone C: Build/Test/Run Plumbing ✅

**Goal:** Core build workflow via Cabal

### Tasks
- [x] Implement `hx build`:
  - [x] Resolve project root
  - [x] Ensure toolchain present (or fail with fix)
  - [x] Call `cabal build` with stable directories:
    - `--store-dir ~/.cache/hx/cabal/store`
    - `--builddir .hx/cabal/dist-newstyle`
  - [x] Capture and parse output
  - [x] Show time + summary
- [x] Implement `hx test`:
  - [x] Call `cabal test`
  - [x] Parse test output
  - [x] Show summary
- [x] Implement `hx run`:
  - [x] Build first
  - [x] Find and execute binary
  - [x] Pass through args
- [x] Implement `hx repl`:
  - [x] Call `cabal repl`
  - [x] Pass through to ghci
- [x] Implement `hx check`:
  - [x] Alias to `hx build` (fast typecheck)
- [x] Create friendly error messages for common failures

### Deliverables
- [x] `hx build` compiles a project
- [x] `hx test` runs tests
- [x] `hx run` executes the binary
- [x] `hx repl` opens ghci

---

## Milestone D: Project Initialization ✅

**Goal:** Create new Haskell projects with sane defaults

### Tasks
- [x] Implement `hx init`:
  - [x] `--bin` flag for executable project
  - [x] `--lib` flag for library project
  - [x] `--name <name>` for project name
  - [x] `--dir <path>` for target directory
- [x] Generate files:
  - [x] `hx.toml` with sensible defaults
  - [x] `.gitignore` with `.hx/` entry
  - [x] Minimal `.cabal` file
  - [x] `src/Main.hs` (for bin) or `src/Lib.hs` (for lib)
  - [x] `.editorconfig`
- [x] Optional: generate `.github/workflows/ci.yml` (--ci flag)
- [x] Infer toolchain versions from environment

### Deliverables
- [x] `hx init --bin --name hello` creates buildable project
- [x] Generated project passes `hx build`

---

## Milestone E: Lock/Sync (Core Product) ✅

**Goal:** Deterministic, reproducible builds

### Tasks
- [x] Implement `hx lock`:
  - [x] Run `cabal update`
  - [x] Run `cabal build --dry-run` to generate plan
  - [x] Run `cabal freeze` to pin dependencies
  - [x] Parse freeze file and/or plan
  - [x] Extract:
    - Compiler ID
    - Platform
    - Index state
    - Package list with versions
  - [x] Compute fingerprint hash
  - [x] Write `hx.lock` (TOML format)
- [x] Implement `hx sync`:
  - [x] Verify `hx.lock` exists
  - [x] Verify toolchain matches lock
  - [x] Offer automatic toolchain install if mismatch
  - [x] Apply constraints from lock
  - [x] Build with frozen dependencies
- [x] Implement fingerprint calculation:
  ```
  fingerprint = sha256(
    toolchain(ghc, cabal) +
    platform(triple) +
    package list (name@version) +
    flags +
    index-state +
    freeze constraints
  )
  ```

### hx.lock Format
```toml
version = 1
created_at = "2026-01-15T00:00:00Z"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"

[plan]
compiler_id = "ghc-9.8.2"
platform = "x86_64-linux"
index_state = "2026-01-15T00:00:00Z"
hash = "sha256:..."

[[packages]]
name = "text"
version = "2.1.1"
source = "hackage"
hash = "sha256:..."
```

### Deliverables
- [x] `hx lock` creates `hx.lock`
- [x] `hx sync` enforces lock during build
- [x] Builds are reproducible given same lock

---

## Milestone F: Format/Lint ✅

**Goal:** Integrated code quality tools

### Tasks
- [x] Implement `hx fmt`:
  - [x] Detect fourmolu in PATH (preferred)
  - [x] Fall back to ormolu
  - [x] Run across all `.hs` files
  - [x] Support `--check` mode
- [x] Implement `hx lint`:
  - [x] Detect hlint in PATH
  - [x] Run with baseline config
  - [x] Parse and format output
  - [x] Support `--fix` for auto-fixes

### Deliverables
- [x] `hx fmt` formats all Haskell files
- [x] `hx lint` reports issues with suggestions

---

## Milestone G: Doctor-Grade Diagnostics ✅

**Goal:** World-class error messages and fixes

### Tasks
- [x] Enhance `hx doctor`:
  - [x] `hx.toml` present check
  - [x] `.cabal` file exists check
  - [x] ghcup/ghc/cabal versions OK check
  - [x] HLS matches GHC major/minor check
  - [x] `cabal --version` sanity check
- [x] Platform-specific native dependency checks:
  - [x] Linux: check for libgmp, libz, libncurses, libffi (pkg-config + paths)
  - [x] macOS: check via brew/pkg-config, Xcode CLI tools
  - [x] Windows: MSYS2 detection and library checks
- [x] Better Cabal error extraction:
  - [x] Parse common error patterns
  - [x] Identify root cause
  - [x] Suggest specific fixes
- [x] Generate "fix commands" automatically
- [x] Summary report output with priorities

### Deliverables
- [x] `hx doctor` identifies all common setup issues
- [x] Each issue has an actionable fix command
- [x] Output is clear and prioritized

---

## Bonus: UV-Inspired Improvements ✅

Applied patterns from the uv (Astral) codebase:

- [x] `hx-warnings` crate with `warn_user!` and `warn_user_once!` macros
- [x] `Printer` abstraction with Silent/Quiet/Normal/Verbose modes
- [x] `EnvVars` constants for environment variable configuration
- [x] `Combine` trait for config merging
- [x] Custom CLI styling with clap styles
- [x] `GlobalArgs` with verbose/quiet/no-color/config-file flags
- [x] Error chain formatting with `write_error_chain`

---

## Bonus: CI/CD & Testing ✅

- [x] GitHub Actions workflow (lint, test matrix, build, docs)
- [x] Integration test infrastructure with assert_cmd
- [x] 26 passing tests

---

## v0.2.0 - Performance & Caching ✅

### Performance & Caching
- [x] Shared build store keyed by fingerprints
  - `StoreIndex` tracks builds by SHA256 fingerprint
  - `hx cache status` shows cache statistics
  - `hx cache prune` removes old entries
  - `hx cache clean` clears entire cache
- [x] Smarter incremental builds
  - Source fingerprinting (hashes all .hs, .cabal, hx.toml files)
  - Build state tracking in `.hx/build-state.json`
  - No-op detection skips `cabal build` when nothing changed
  - Shows "Fresh" status for unchanged projects (~100ms)
- [x] Better parallel fetch
  - New `hx fetch` command for pre-downloading dependencies
  - Uses `cabal fetch -j8` for parallel downloads
  - Integrates with lockfile for exact version fetching

### Workspace Support
- [x] Multi-package repos (cabal.project parsing)
- [x] Consistent `hx.lock` for workspaces
  - Single lockfile at workspace root covers all packages
  - Stores workspace package info (name, version, path)
  - Filters local packages from external dependencies
  - Validates workspace consistency on `hx sync`
- [x] Workspace-aware commands (--package flag)

### Toolchain Management
- [x] `hx toolchain install` via ghcup
- [x] `hx toolchain use <profile>` for per-project pins
- [x] Automatic toolchain installation on mismatch
  - Checks toolchain requirements from hx.toml before build/test/run/bench
  - Interactive prompt for installation (respects CI/non-TTY)
  - `--auto-install` flag for unattended installation
  - `--no-auto-install` flag to disable prompts

---

## v0.3.0 - Replace Selected Cabal Parts ✅

### Lock Resolution Layer
- [x] Custom dependency resolver (`hx-solver` crate)
  - [x] PVP version parsing and constraints (==, >=, >, <, <=, ^>=, &&, ||)
  - [x] Hackage 01-index.tar.gz parsing
  - [x] .cabal file parsing for build-depends extraction
  - [x] Backtracking dependency resolution algorithm
  - [x] Cycle detection in dependency graphs
  - [x] Native solver is now the default (`hx lock --cabal` for old behavior)
- [x] Offline solver cache
  - [x] Binary caching of PackageIndex (bincode serialization)
  - [x] Resolution result caching keyed by dependency fingerprint
  - [x] Automatic cache invalidation when source changes
- [ ] Controlled Hackage index mirroring (future)
  - Local package index management
  - Faster index updates
  - Offline-capable resolution

---

## Command Namespace (Final)

```
hx init [--bin|--lib] [--ci]
hx build [--package <name>]
hx test [--package <name>]
hx run [--package <name>]
hx repl
hx check

hx new module|test|benchmark <name>
hx bench [--package <name>]
hx publish [--dry-run] [--docs]

hx fmt [--check]
hx lint [--fix]
hx doctor

hx lock [--cabal]
hx fetch [-j <jobs>]
hx sync
hx clean [--global]
hx cache status|prune|clean

hx add <package>
hx rm <package>

hx ide setup|status

hx toolchain status
hx toolchain install
hx toolchain use

hx completions <shell>
hx upgrade [--check]
```

---

## Opinionated Defaults

| Setting | Default |
|---------|---------|
| Formatter | fourmolu |
| Linter | hlint |
| Build system | cabal v2 |
| Store/build dirs | hx-managed |
| Lockfile format | TOML |
| Error output | short + fixes |
| Verbose output | behind `--verbose` |
