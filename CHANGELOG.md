# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1] - 2026-01-16

### Added
- `hx completions <shell>` - Generate shell completions for bash, zsh, fish, PowerShell
- `hx upgrade` - Self-update to latest version from GitHub releases
- `hx upgrade --check` - Check for updates without installing
- `hx upgrade --target <version>` - Install a specific version

### Improved
- Enhanced error messages with actionable fix suggestions
- Added convenience error constructors with intelligent default fixes
- Toolchain missing errors now suggest both ghcup and hx commands
- Build errors analyze content to suggest relevant fixes

## [0.1.0] - 2026-01-15

### Added

#### Core Commands
- `hx init` - Initialize new Haskell projects with `--bin`, `--lib`, `--name`, `--dir` options
- `hx build` - Build projects via Cabal with `--release`, `--jobs`, `--target` options
- `hx test` - Run tests with optional `--pattern` matching
- `hx run` - Build and run executables, passing arguments through
- `hx repl` - Start an interactive GHCi session
- `hx check` - Fast type-checking (alias to build)
- `hx clean` - Clean build artifacts with `--global` option

#### Dependency Management
- `hx lock` - Generate deterministic lockfile (`hx.lock`) with package versions and fingerprints
- `hx sync` - Build with locked dependencies, verify toolchain compatibility
- `hx add` - Add dependencies to `.cabal` file
- `hx rm` - Remove dependencies from `.cabal` file

#### Code Quality
- `hx fmt` - Format Haskell source files with fourmolu/ormolu, supports `--check` mode
- `hx lint` - Lint with hlint, supports `--fix` for auto-fixes

#### Toolchain Management
- `hx toolchain status` - Show installed GHC, Cabal, GHCup, HLS versions
- `hx toolchain install` - Install toolchain components via GHCup
- `hx toolchain use` - Switch active toolchain version or use project settings

#### Diagnostics
- `hx doctor` - Comprehensive diagnostics with actionable fix suggestions

#### Configuration
- `hx.toml` manifest format with project, toolchain, format, lint sections
- `hx.lock` TOML lockfile format with fingerprint verification
- Environment variable support: `HX_VERBOSE`, `HX_QUIET`, `HX_NO_COLOR`, `HX_CONFIG_FILE`, etc.

#### Developer Experience
- Global `--verbose`, `--quiet`, `--no-color`, `--config-file` flags
- Colored output with automatic terminal detection
- Progress spinners for long-running operations
- Structured error messages with fix suggestions
- Warning system with deduplication (`warn_user_once!`)

### Architecture
- Rust workspace with 11 crates: `hx-cli`, `hx-core`, `hx-config`, `hx-lock`, `hx-toolchain`, `hx-cabal`, `hx-cache`, `hx-doctor`, `hx-ui`, `hx-telemetry`, `hx-warnings`
- Async runtime with Tokio
- Structured logging with tracing
- UV-inspired patterns: Printer abstraction, Combine trait, EnvVars constants

### Testing
- 26 automated tests
- Integration test infrastructure with assert_cmd
- CI/CD with GitHub Actions (Linux, macOS, Windows)

[Unreleased]: https://github.com/raskell-io/hx/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/raskell-io/hx/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/raskell-io/hx/releases/tag/v0.1.0
