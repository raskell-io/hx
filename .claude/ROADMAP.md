# HX Roadmap

## v0.1.0 - Initial Release

### Done Definition

hx v0.1.0 is shippable when:
- `hx init` creates a project that builds on at least Linux/macOS
- `hx toolchain status` works robustly
- `hx build/test/run/repl` work for a basic project
- `hx lock` produces deterministic lockfile
- `hx sync` enforces the lock
- `hx doctor` gives actionable fixes for missing tools/mismatch

---

## Milestone A: Scaffolding

**Goal:** Basic project structure and CLI framework

### Tasks
- [ ] Create Rust workspace with all crates:
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
- [ ] Implement `hx --version`
- [ ] Implement `hx help`
- [ ] Implement config loading (hx.toml parsing)
- [ ] Implement project root detection
- [ ] Add structured logging with `tracing`
- [ ] Add `hx-ui` output helpers (spinners, progress)

### Deliverables
- Workspace compiles
- `cargo run -p hx-cli -- --version` works
- `cargo run -p hx-cli -- help` shows command list

---

## Milestone B: Toolchain Detection

**Goal:** Detect and report installed Haskell tools

### Tasks
- [ ] Implement `hx toolchain status`
- [ ] Detect `ghc` on PATH, parse version
- [ ] Detect `cabal` on PATH, parse version
- [ ] Detect `ghcup` on PATH, parse version
- [ ] Detect `hls` (haskell-language-server-wrapper), parse version
- [ ] Implement basic `hx doctor`:
  - [ ] Missing tool detection
  - [ ] Version mismatch warnings
  - [ ] Actionable fix suggestions

### Deliverables
- `hx toolchain status` shows all detected tools
- `hx doctor` identifies missing/mismatched tools

---

## Milestone C: Build/Test/Run Plumbing

**Goal:** Core build workflow via Cabal

### Tasks
- [ ] Implement `hx build`:
  - [ ] Resolve project root
  - [ ] Ensure toolchain present (or fail with fix)
  - [ ] Call `cabal build` with stable directories:
    - `--store-dir ~/.cache/hx/cabal/store`
    - `--builddir .hx/cabal/dist-newstyle`
  - [ ] Capture and parse output
  - [ ] Show time + summary
- [ ] Implement `hx test`:
  - [ ] Call `cabal test`
  - [ ] Parse test output
  - [ ] Show summary
- [ ] Implement `hx run`:
  - [ ] Build first
  - [ ] Find and execute binary
  - [ ] Pass through args
- [ ] Implement `hx repl`:
  - [ ] Call `cabal repl`
  - [ ] Pass through to ghci
- [ ] Implement `hx check`:
  - [ ] Alias to `hx build` (fast typecheck)
- [ ] Create friendly error messages for common failures

### Deliverables
- `hx build` compiles a project
- `hx test` runs tests
- `hx run` executes the binary
- `hx repl` opens ghci

---

## Milestone D: Project Initialization

**Goal:** Create new Haskell projects with sane defaults

### Tasks
- [ ] Implement `hx init`:
  - [ ] `--bin` flag for executable project
  - [ ] `--lib` flag for library project
  - [ ] `--name <name>` for project name
  - [ ] `--dir <path>` for target directory
- [ ] Generate files:
  - [ ] `hx.toml` with sensible defaults
  - [ ] `.gitignore` with `.hx/` entry
  - [ ] Minimal `.cabal` file
  - [ ] `src/Main.hs` (for bin) or `src/Lib.hs` (for lib)
  - [ ] `.editorconfig`
- [ ] Optional: generate `.github/workflows/ci.yml`
- [ ] Infer toolchain versions from environment

### Deliverables
- `hx init --bin --name hello` creates buildable project
- Generated project passes `hx build`

---

## Milestone E: Lock/Sync (Core Product)

**Goal:** Deterministic, reproducible builds

### Tasks
- [ ] Implement `hx lock`:
  - [ ] Run `cabal update`
  - [ ] Run `cabal build --dry-run` to generate plan
  - [ ] Run `cabal freeze` to pin dependencies
  - [ ] Parse freeze file and/or plan
  - [ ] Extract:
    - Compiler ID
    - Platform
    - Index state
    - Package list with versions
  - [ ] Compute fingerprint hash
  - [ ] Write `hx.lock` (TOML format)
- [ ] Implement `hx sync`:
  - [ ] Verify `hx.lock` exists
  - [ ] Verify toolchain matches lock
  - [ ] Offer automatic toolchain install if mismatch
  - [ ] Apply constraints from lock
  - [ ] Build with frozen dependencies
- [ ] Implement fingerprint calculation:
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
- `hx lock` creates `hx.lock`
- `hx sync` enforces lock during build
- Builds are reproducible given same lock

---

## Milestone F: Format/Lint

**Goal:** Integrated code quality tools

### Tasks
- [ ] Implement `hx fmt`:
  - [ ] Detect fourmolu in PATH (preferred)
  - [ ] Fall back to ormolu
  - [ ] Run across all `.hs` files
  - [ ] Support `--check` mode
- [ ] Implement `hx lint`:
  - [ ] Detect hlint in PATH
  - [ ] Run with baseline config
  - [ ] Parse and format output
  - [ ] Support `--fix` for auto-fixes

### Deliverables
- `hx fmt` formats all Haskell files
- `hx lint` reports issues with suggestions

---

## Milestone G: Doctor-Grade Diagnostics

**Goal:** World-class error messages and fixes

### Tasks
- [ ] Enhance `hx doctor`:
  - [ ] `hx.toml` present check
  - [ ] `.cabal` file exists check
  - [ ] ghcup/ghc/cabal versions OK check
  - [ ] HLS matches GHC major/minor check
  - [ ] `cabal --version` sanity check
- [ ] Platform-specific native dependency checks:
  - [ ] Linux: check for libgmp, libz, libncurses
  - [ ] macOS: check via brew/pkg-config
  - [ ] Windows: basic checks
- [ ] Better Cabal error extraction:
  - [ ] Parse common error patterns
  - [ ] Identify root cause
  - [ ] Suggest specific fixes
- [ ] Generate "fix commands" automatically
- [ ] Summary report output with priorities

### Deliverables
- `hx doctor` identifies all common setup issues
- Each issue has an actionable fix command
- Output is clear and prioritized

---

## Future: v1.0

### Performance & Caching
- [ ] Shared build store keyed by fingerprints
- [ ] Smarter incremental builds
- [ ] Better parallel fetch

### Workspace Support
- [ ] Multi-package repos
- [ ] Consistent `hx.lock` for workspaces
- [ ] Workspace-aware commands

### Toolchain Management
- [ ] `hx toolchain install` via ghcup
- [ ] `hx toolchain use <profile>` for per-project pins
- [ ] Automatic toolchain installation on mismatch

---

## Future: v2.0

### Replace Selected Cabal Parts
- [ ] Lock resolution layer
- [ ] Offline solver cache
- [ ] Controlled Hackage index mirroring

---

## Command Namespace (Final)

```
hx init
hx build
hx test
hx run
hx repl
hx check

hx fmt
hx lint
hx doctor

hx lock
hx sync
hx clean

hx toolchain status
hx toolchain install
hx toolchain use
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
