# Diagnostic Checks

`hx-doctor` performs comprehensive checks across toolchain, native dependencies, and project configuration.

## Check Categories

### Toolchain Checks

Verify that Haskell development tools are properly installed and configured.

#### GHCup Check

```rust
fn check_ghcup(toolchain: &Toolchain, report: &mut DoctorReport)
```

| Condition | Severity | Message |
|-----------|----------|---------|
| Not found | Warning | "ghcup not found - cannot manage toolchain versions" |
| Found | Info | "ghcup: {version}" |

**Fix**: Install ghcup from https://www.haskell.org/ghcup/

#### GHC Check

```rust
fn check_ghc(toolchain: &Toolchain, report: &mut DoctorReport)
```

| Condition | Severity | Message |
|-----------|----------|---------|
| Not found | Error | "ghc not found" |
| Found | Info | "ghc: {version}" |

**Fixes**:
- `ghcup install ghc`
- `hx toolchain install --ghc 9.8.2`

#### Cabal Check

```rust
fn check_cabal(toolchain: &Toolchain, report: &mut DoctorReport)
```

| Condition | Severity | Message |
|-----------|----------|---------|
| Not found | Error | "cabal not found" |
| Found | Info | "cabal: {version}" |

**Fix**: `ghcup install cabal`

#### HLS Check

```rust
fn check_hls(toolchain: &Toolchain, report: &mut DoctorReport)
```

| Condition | Severity | Message |
|-----------|----------|---------|
| Not found | Warning | "haskell-language-server not found - IDE features unavailable" |
| Incompatible | Warning | "HLS {version} may not be fully compatible with GHC {version}" |
| Found | Info | "hls: {version}" |

**Fixes**:
- `ghcup install hls`
- `hx toolchain install --hls latest`

### Native Dependency Checks

Platform-specific checks for required native libraries.

#### Linux

```rust
fn check_native_deps_linux(report: &mut DoctorReport)
```

Uses `pkg-config`, `ldconfig`, and direct path checks:

| Library | Description | Debian Package | Fedora Package | Arch Package |
|---------|-------------|----------------|----------------|--------------|
| gmp | GMP arithmetic | `libgmp-dev` | `gmp-devel` | `gmp` |
| zlib | Compression | `zlib1g-dev` | `zlib-devel` | `zlib` |
| ncurses | Terminal | `libncurses-dev` | `ncurses-devel` | `ncurses` |
| libffi | FFI | `libffi-dev` | `libffi-devel` | `libffi` |

Detection paths:
```
/usr/lib/x86_64-linux-gnu/lib{name}.so
/usr/lib64/lib{name}.so
/usr/lib/aarch64-linux-gnu/lib{name}.so
/usr/lib/lib{name}.so
/lib/lib{name}.so
```

#### macOS

```rust
fn check_native_deps_macos(report: &mut DoctorReport)
```

Checks Xcode CLI Tools and Homebrew packages:

| Dependency | Check Method | Fix |
|------------|--------------|-----|
| Xcode CLI | `xcode-select --print-path` | `xcode-select --install` |
| Homebrew | `brew --version` | Install from brew.sh |
| gmp | `brew list gmp` | `brew install gmp` |
| libffi | `brew list libffi` | `brew install libffi` |

Detection paths:
```
/opt/homebrew/lib/lib{name}.dylib
/usr/local/lib/lib{name}.dylib
/opt/homebrew/opt/{name}/lib/lib{name}.dylib
/usr/local/opt/{name}/lib/lib{name}.dylib
```

#### Windows

```rust
fn check_native_deps_windows(report: &mut DoctorReport)
```

Checks MSYS2/MinGW installation:

| Check | Paths |
|-------|-------|
| MSYS2 | `C:\msys64`, `C:\msys32`, `C:\ghcup\msys64` |
| Libraries | `{msys2}\mingw64\lib\lib{name}.a` |

Required libraries: `libgmp`, `libffi`, `libz`

### Project Checks

Verify project configuration and structure.

#### Manifest Check

```rust
fn check_project(dir: &Path, report: &mut DoctorReport)
```

| Condition | Severity | Message | Fix |
|-----------|----------|---------|-----|
| No hx.toml | Warning | "hx.toml not found" | `hx init` |
| No .cabal | Error | "no .cabal file found" | `hx init` |

#### IDE Configuration Check

```rust
fn check_hie_yaml(dir: &Path, report: &mut DoctorReport)
```

| Status | Severity | Message | Fix |
|--------|----------|---------|-----|
| Missing (workspace) | Warning | "hie.yaml missing - HLS may not work correctly" | `hx ide setup` |
| Outdated | Warning | "hie.yaml is outdated" | `hx ide setup --force` |
| Up to date | Info | "hie.yaml up to date" | - |
| Exists (custom) | Info | "hie.yaml found (custom configuration)" | - |

## Check Flow

```
run_checks(project_dir)
    │
    ├── Toolchain::detect()
    │
    ├── check_ghcup()
    ├── check_ghc()
    ├── check_cabal()
    ├── check_hls()
    │
    ├── check_native_deps()
    │   └── Platform-specific checks
    │
    └── check_project()
        ├── Check hx.toml
        ├── Check .cabal files
        └── check_hie_yaml()
```

## Extending Checks

Add custom checks by modifying the report:

```rust
let mut report = run_checks(Some(&project_dir)).await;

// Add custom check
if !project_dir.join("stack.yaml").exists() {
    report.add(Diagnostic::info("No stack.yaml found (using cabal)"));
}

// Add environment check
if std::env::var("GHCUP_USE_XDG_DIRS").is_ok() {
    report.add(Diagnostic::info("Using XDG directory layout for ghcup"));
}
```

## Exit Codes

The `hx doctor` command returns:

| Code | Meaning |
|------|---------|
| 0 | No errors (warnings/info OK) |
| 1 | One or more errors found |

```rust
let (errors, _, _) = report.counts();
std::process::exit(if errors > 0 { 1 } else { 0 });
```
