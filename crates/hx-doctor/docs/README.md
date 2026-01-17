# hx-doctor

Diagnostic checks and fix suggestions for hx projects.

## Overview

`hx-doctor` provides the `hx doctor` command to diagnose:

- Toolchain installation issues
- Version mismatches
- Native dependency problems
- Project configuration errors
- IDE setup issues

## Quick Start

```rust
use hx_doctor::{run_checks, DoctorReport, Diagnostic};
use hx_ui::Output;

let output = Output::new();
let report = run_checks(&project_root, &output).await?;

if report.has_errors() {
    println!("Found {} errors", report.error_count());
}

print_report(&report, &output);
```

## DoctorReport

```rust
pub struct DoctorReport {
    /// All diagnostics found
    pub diagnostics: Vec<Diagnostic>,
}

impl DoctorReport {
    /// Check if any errors were found
    pub fn has_errors(&self) -> bool;

    /// Check if any warnings were found
    pub fn has_warnings(&self) -> bool;

    /// Get (errors, warnings, info) counts
    pub fn counts(&self) -> (usize, usize, usize);

    /// Add a diagnostic
    pub fn add(&mut self, diagnostic: Diagnostic);
}
```

## Diagnostic

```rust
pub struct Diagnostic {
    /// Severity level
    pub severity: Severity,

    /// Problem description
    pub message: String,

    /// Suggested fixes
    pub fixes: Vec<Fix>,
}

pub enum Severity {
    Info,     // Informational
    Warning,  // Non-blocking issue
    Error,    // Blocking problem
}

impl Diagnostic {
    /// Create an error diagnostic
    pub fn error(message: impl Into<String>) -> Self;

    /// Create a warning diagnostic
    pub fn warning(message: impl Into<String>) -> Self;

    /// Create an info diagnostic
    pub fn info(message: impl Into<String>) -> Self;

    /// Add a suggested fix
    pub fn with_fix(self, fix: Fix) -> Self;
}
```

## Checks Performed

### Toolchain Checks

| Check | Error | Fix |
|-------|-------|-----|
| GHCup not found | Error | Install ghcup |
| GHC not found | Error | `hx toolchain install` |
| GHC version mismatch | Error | Install required version |
| Cabal not found | Error | `hx toolchain install` |
| Cabal version mismatch | Warning | Update Cabal |
| HLS not found | Warning | `ghcup install hls` |
| HLS/GHC incompatible | Warning | Install compatible HLS |

### Native Dependencies (Linux)

| Dependency | Package |
|------------|---------|
| gmp | `libgmp-dev` |
| ncurses | `libncurses-dev` |
| libffi | `libffi-dev` |
| zlib | `zlib1g-dev` |

### Native Dependencies (macOS)

| Dependency | Source |
|------------|--------|
| Xcode CLI Tools | `xcode-select --install` |
| gmp | `brew install gmp` |
| libffi | `brew install libffi` |

### Native Dependencies (Windows)

| Dependency | Source |
|------------|--------|
| MSYS2 | msys2.org |
| MinGW-w64 | Via MSYS2 |

### Project Checks

| Check | Level | Fix |
|-------|-------|-----|
| No hx.toml | Error | `hx init` |
| No .cabal file | Warning | Create .cabal |
| hie.yaml missing | Info | `hx ide setup` |
| hie.yaml outdated | Warning | `hx ide setup` |

## Running Checks

```rust
use hx_doctor::run_checks;

let report = run_checks(&project_root, &output).await?;

for diag in &report.diagnostics {
    match diag.severity {
        Severity::Error => output.error(&diag.message),
        Severity::Warning => output.warn(&diag.message),
        Severity::Info => output.info(&diag.message),
    }

    for fix in &diag.fixes {
        if let Some(cmd) = &fix.command {
            output.info(&format!("  fix: {}", cmd));
        } else {
            output.info(&format!("  fix: {}", fix.description));
        }
    }
}
```

## Example Output

```
hx doctor

Checking toolchain...
  ✓ ghcup 0.1.30.0
  ✓ ghc 9.8.2
  ✓ cabal 3.12.1.0
  ✗ hls not found

    warning: HLS not found
    fix: Install HLS with `ghcup install hls`

Checking native dependencies...
  ✓ gmp
  ✓ ncurses
  ✓ libffi

Checking project...
  ✓ hx.toml
  ✓ my-project.cabal
  ! hie.yaml missing

    info: hie.yaml not found (IDE features may not work)
    fix: Run `hx ide setup` to generate

Summary: 0 errors, 1 warning, 1 info
```

## Printing Reports

```rust
use hx_doctor::print_report;

let report = run_checks(&project_root, &output).await?;
print_report(&report, &output);

// Exit with appropriate code
let (errors, _, _) = report.counts();
std::process::exit(if errors > 0 { 1 } else { 0 });
```

## Custom Checks

Add project-specific checks:

```rust
let mut report = run_checks(&project_root, &output).await?;

// Add custom check
if !project_root.join("README.md").exists() {
    report.add(Diagnostic::info("README.md not found")
        .with_fix(Fix::new("Create a README for your project")));
}
```

## Documentation

- [Diagnostic Checks](./checks.md) - Details on all checks performed
