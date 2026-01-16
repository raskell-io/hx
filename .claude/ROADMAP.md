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

### Full Native Build Replacement ✅
- [x] Complete GHC invocation without cabal dependency
  - [x] Enhanced cabal file parsing (extensions, source dirs, c-sources, ghc-options)
  - [x] Package source extraction from Hackage tarballs
  - [x] Native package compilation from source with GHC
  - [x] Library creation with ar/libtool
  - [x] Package registration file generation
  - [x] Package database management with ghc-pkg
  - [x] Full dependency build orchestration (FullNativeBuilder)
  - [x] Topological ordering of package builds
  - [x] Pre-installed package detection
  - [x] Package cache with content-addressed keys
- [x] Drop cabal as runtime dependency for simple builds
- [x] Constraints: Custom Setup.hs and preprocessors (alex/happy) require cabal fallback

### Watch Mode ✅
- [x] `hx watch` for auto-rebuild on file changes
  - [x] File system monitoring (notify crate)
  - [x] Debounced rebuilds
  - [x] Test re-run on change
  - [x] Clear terminal between runs

### Dependency Visualization ✅
- [x] `hx deps graph` command
  - [x] Graphviz DOT output
  - [x] ASCII tree fallback
  - [x] JSON output format
  - [x] Simple list format
  - [x] Filter by depth
  - [x] Highlight specific packages
  - [x] Direct-only filter
- [x] `hx deps tree` convenience alias
- [x] `hx deps list` convenience alias

### Language Server Features ✅
- [x] Built-in diagnostics without HLS
  - [x] Type error extraction from GHC
  - [x] Warning aggregation
  - [x] Quick-fix suggestions
- [x] `hx lsp` for minimal language server

### Plugin System ✅
- [x] User-defined commands via hooks
  - [x] Pre/post build, test, run, clean, lock hooks
  - [x] Custom command registration
  - [x] Steel (Scheme) plugin runtime
  - [x] `hx plugins list|status|run` commands
  - [x] Plugin API for project info, shell commands, file operations
  - [x] Thread-local context for implicit plugin state access

### Cloud Builds
- [ ] Remote build execution
  - [ ] Build server protocol
  - [ ] Distributed compilation
  - [ ] Result caching and sharing

### GHC Version Manager ✅
- [x] Replace ghcup dependency
  - [x] Direct GHC binary downloads from downloads.haskell.org
  - [x] Version switching with `hx toolchain use`
  - [x] Per-project GHC installations via hx.toml
  - [x] `hx toolchain list` with --available/--installed flags
  - [x] `hx toolchain install <version>` with --set/--force/--ghcup options
  - [x] `hx toolchain remove <version>` with confirmation
  - [x] PATH injection for builds using resolved GHC
  - [x] Graceful fallback to ghcup when direct download fails

### Binary Distribution ✅
- [x] `hx dist` command
  - [x] Build release binary for target platform
  - [x] Strip debug symbols
  - [x] Create tar.gz/zip archives with metadata
  - [x] Bundle shell completions (bash, fish, zsh, powershell)
  - [x] SHA256 checksum generation
- [x] `hx dist formula` - Generate Homebrew formula
- [x] `hx dist install-script` - Generate shell installation script

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
hx toolchain list [--available] [--installed]
hx toolchain install <version> [--set] [--force] [--ghcup]
hx toolchain remove <version> [-y]
hx toolchain use <version>

hx index update [--force]
hx index status
hx index clear

hx nix flake|shell [--output <file>]
hx profile [--heap] [--time] <args>
hx script <file.hs> [args]
hx import --from stack|cabal
hx search <query> [--limit <n>]
hx audit [--fix] [--ignore <advisory>]
hx watch [--test] [--clear] [--debounce <ms>]

hx deps graph [--format dot|tree|list|json] [--depth <n>] [--highlight <pkgs>]
hx deps tree [--depth <n>]
hx deps list [--direct]

hx lsp [--tcp <port>]

hx plugins list
hx plugins status
hx plugins run <script> [args]

hx dist [--target <triple>] [--output <dir>] [--strip] [--completions]
hx dist formula [--version <ver>] [--output <file>]
hx dist install-script [--version <ver>] [--output <file>]

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
