# HX Roadmap

## v0.1.0 - Initial Release ✅

### Done Definition

hx v0.1.0 is shippable when:
- [x] `hx init` creates a project that builds on at least Linux/macOS
- [x] `hx toolchain status` works robustly
- [x] `hx build/test/run/repl` work for a basic project
- [x] `hx lock` produces deterministic lockfile
- [x] `hx sync` enforces the lock
- [x] `hx doctor` gives actionable fixes for missing tools/mismatch

### Milestone A: Scaffolding ✅
- [x] Create Rust workspace with all crates
- [x] Implement `hx --version`, `hx help`
- [x] Config loading (hx.toml parsing)
- [x] Project root detection
- [x] Structured logging with `tracing`
- [x] UI helpers (spinners, progress)

### Milestone B: Toolchain Detection ✅
- [x] `hx toolchain status` - detect ghc, cabal, ghcup, hls
- [x] `hx doctor` with actionable fixes

### Milestone C: Build/Test/Run ✅
- [x] `hx build`, `hx test`, `hx run`, `hx repl`, `hx check`
- [x] Stable build directories
- [x] Friendly error messages

### Milestone D: Project Initialization ✅
- [x] `hx init` with --bin, --lib, --name, --dir, --ci flags
- [x] Generate hx.toml, .gitignore, .cabal, source files

### Milestone E: Lock/Sync ✅
- [x] `hx lock` - generate hx.lock with fingerprint
- [x] `hx sync` - enforce lockfile during build

### Milestone F: Format/Lint ✅
- [x] `hx fmt` (fourmolu/ormolu)
- [x] `hx lint` (hlint with --fix)

### Milestone G: Doctor Diagnostics ✅
- [x] Platform-specific native dependency checks
- [x] Better Cabal error extraction
- [x] Actionable fix commands

---

## v0.2.0 - Feature Complete ✅

### Performance & Caching
- [x] Shared build store keyed by fingerprints
- [x] `hx cache status|prune|clean` commands
- [x] Source fingerprinting for incremental builds
- [x] `hx fetch` for parallel dependency downloads

### Workspace Support
- [x] Multi-package repos (cabal.project parsing)
- [x] Single lockfile for workspaces
- [x] `--package` flag for workspace commands

### Toolchain Management
- [x] `hx toolchain install` via ghcup
- [x] `hx toolchain use` for per-project pins
- [x] Automatic toolchain installation on mismatch

### Native Dependency Resolver
- [x] `hx-solver` crate with PVP version parsing
- [x] Hackage 01-index.tar.gz parsing
- [x] Backtracking dependency resolution
- [x] Cycle detection
- [x] Binary caching of PackageIndex

### Native Package Fetching
- [x] Direct Hackage downloads with parallel concurrency
- [x] SHA256 verification
- [x] Progress tracking with ETA

### Build Plan Generation
- [x] Topological sorting of dependencies
- [x] Pre-installed package detection
- [x] Build fingerprint calculation

### Hackage Index Mirroring
- [x] `hx index update|status|clear`
- [x] HTTP conditional requests (ETag, If-Modified-Since)
- [x] Auto-update when stale

### Project Templates
- [x] `hx new webapp|cli|library`
- [x] `hx new --template <git-url>`
- [x] Template variable substitution

### Artifact Caching
- [x] Content-addressed storage for .o/.hi files
- [x] `hx cache artifacts status|prune|clear`

### Module Dependency Analysis
- [x] Import statement parsing
- [x] Topological ordering for compilation
- [x] Parallel compilation grouping

### Native Build Orchestration
- [x] `hx build --native` for direct GHC invocation
- [x] Module dependency graph extraction
- [x] Parallel compilation with dependency tracking
- [x] Package database integration

### Documentation Generation
- [x] `hx docs` with --open, --deps, --serve flags

### IDE Integration
- [x] `hx ide setup|status`
- [x] Auto-generate hie.yaml
- [x] HLS version compatibility checks

### Package Publishing
- [x] `hx publish` with pre-publish checks
- [x] Hackage upload, dry-run mode, --docs flag

### Changelog Generation
- [x] `hx changelog` from git commits
- [x] Conventional commits support
- [x] --preview and --unreleased flags

### Nix Integration
- [x] `hx nix flake|shell` for Nix file generation
- [x] Generate flake.nix/shell.nix from hx.toml

### Profiling Integration
- [x] `hx profile` with --heap, --time flags
- [x] Build with -prof, run with RTS options
- [x] Parse and display profiling output

### Single-File Scripts
- [x] `hx script <file.hs>`
- [x] Parse dependencies from file header
- [x] Cache compiled scripts

### Stack Project Import
- [x] `hx import --from stack|cabal`
- [x] Parse stack.yaml, convert resolver to GHC version

### Package Search
- [x] `hx search <query>` with fuzzy matching
- [x] --limit and --detailed flags

### Dependency Audit
- [x] `hx audit` for vulnerability scanning
- [x] Detect outdated/deprecated packages
- [x] License checking, --fix flag

---

## v0.3.0 - Native Build & Beyond

### Full Native Build Replacement
- [ ] Complete GHC invocation without cabal dependency
  - [ ] Native package compilation from source
  - [ ] Linking and executable generation
  - [ ] Library installation to package database
  - [ ] Full dependency build orchestration
  - [ ] Cabal file generation/modification
- [ ] Drop cabal as runtime dependency for builds
- [ ] Performance parity with cabal builds

### Watch Mode
- [ ] `hx watch` for auto-rebuild on file changes
  - [ ] File system monitoring (notify crate)
  - [ ] Debounced rebuilds
  - [ ] Test re-run on change
  - [ ] Clear terminal between runs

### Dependency Visualization
- [ ] `hx deps graph` command
  - [ ] Graphviz DOT output
  - [ ] ASCII tree fallback
  - [ ] Filter by depth
  - [ ] Highlight specific packages

### Language Server Features
- [ ] Built-in diagnostics without HLS
  - [ ] Type error extraction from GHC
  - [ ] Warning aggregation
  - [ ] Quick-fix suggestions
- [ ] `hx lsp` for minimal language server

### Plugin System
- [ ] User-defined commands via hooks
  - [ ] Pre/post build hooks
  - [ ] Custom command registration
  - [ ] Lua or WASM plugin runtime

### Cloud Builds
- [ ] Remote build execution
  - [ ] Build server protocol
  - [ ] Distributed compilation
  - [ ] Result caching and sharing

### GHC Version Manager
- [ ] Replace ghcup dependency
  - [ ] Direct GHC binary downloads
  - [ ] Version switching
  - [ ] Per-project GHC installations

### Binary Distribution
- [ ] `hx dist` command
  - [ ] Create release tarballs
  - [ ] Static linking options
  - [ ] Cross-platform installers
  - [ ] Homebrew/apt/chocolatey formulas

### Test Coverage
- [ ] `hx coverage` command
  - [ ] Integration with hpc
  - [ ] HTML coverage reports
  - [ ] Coverage thresholds
  - [ ] CI integration

---

## Command Namespace (Current)

```
hx init [--bin|--lib] [--ci]
hx build [--package <name>] [--native] [--target <triple>]
hx test [--package <name>]
hx run [--package <name>]
hx repl
hx check

hx new module|test|benchmark <name>
hx new webapp|cli|library <name>
hx new --template <git-url> <name>
hx bench [--package <name>]
hx docs [--open] [--deps] [--serve]
hx publish [--dry-run] [--docs]
hx changelog [--unreleased] [--output <file>]

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

hx index update [--force]
hx index status
hx index clear

hx nix flake|shell [--output <file>]
hx profile [--heap] [--time] <args>
hx script <file.hs> [args]
hx import --from stack|cabal
hx search <query> [--limit <n>]
hx audit [--fix] [--ignore <advisory>]

hx completions <shell>
hx upgrade [--check]
```

---

## Opinionated Defaults

| Setting | Default |
|---------|---------|
| Formatter | fourmolu |
| Linter | hlint |
| Build system | native (cabal fallback) |
| Store/build dirs | hx-managed |
| Lockfile format | TOML |
| Error output | short + fixes |
| Verbose output | behind `--verbose` |
