# Diagnostics

`hx-cabal` provides GHC diagnostic parsing for IDE integration.

## Overview

GHC outputs diagnostics (errors, warnings) in two formats:
- **Text format** - Traditional human-readable output
- **JSON format** - Structured format (GHC 9.4+)

## JSON Diagnostics

GHC 9.4+ supports `-fdiagnostics-as-json`:

```rust
use hx_cabal::ghc_diagnostics::{parse_ghc_json, supports_json_diagnostics};

if supports_json_diagnostics("9.8.2") {
    let diagnostics = parse_ghc_json(&ghc_output)?;
    for diag in diagnostics {
        println!("{}: {}", diag.severity, diag.message);
    }
}
```

### JSON Format

```json
{
  "version": "1.0",
  "ghc_version": "9.8.2",
  "diagnostics": [
    {
      "span": {
        "file": "src/Main.hs",
        "start": {"line": 10, "column": 5},
        "end": {"line": 10, "column": 15}
      },
      "severity": "error",
      "code": "GHC-12345",
      "message": "Variable not in scope: undefined"
    }
  ]
}
```

## Text Diagnostics

For older GHC versions, parse text output:

```rust
use hx_cabal::ghc_diagnostics::parse_ghc_text;

let diagnostics = parse_ghc_text(&ghc_output)?;
```

### Text Format

```
src/Main.hs:10:5: error:
    Variable not in scope: undefined

src/Lib.hs:23:1: warning: [-Wunused-imports]
    The import of 'Data.List' is redundant
```

## Diagnostic Types

```rust
pub struct GhcDiagnostic {
    /// Source file
    pub file: PathBuf,

    /// Start position
    pub start_line: u32,
    pub start_col: u32,

    /// End position
    pub end_line: u32,
    pub end_col: u32,

    /// Severity (error, warning, hint)
    pub severity: DiagnosticSeverity,

    /// GHC error code (optional)
    pub code: Option<String>,

    /// Error message
    pub message: String,

    /// Related notes
    pub notes: Vec<String>,
}

pub enum DiagnosticSeverity {
    Error,
    Warning,
    Hint,
    Info,
}
```

## Getting Diagnostic Flags

```rust
use hx_cabal::ghc_diagnostics::diagnostic_flags;

let flags = diagnostic_flags("9.8.2");
// ["-fdiagnostics-as-json", "-fno-diagnostics-show-caret"]
```

## Version Support

```rust
use hx_cabal::ghc_diagnostics::supports_json_diagnostics;

// Check if GHC version supports JSON diagnostics
assert!(supports_json_diagnostics("9.8.2"));
assert!(supports_json_diagnostics("9.4.1"));
assert!(!supports_json_diagnostics("9.2.8"));
```

## LSP Integration

Diagnostics are converted to LSP format for editors:

```rust
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

impl From<GhcDiagnostic> for lsp_types::Diagnostic {
    fn from(diag: GhcDiagnostic) -> Self {
        Diagnostic {
            range: Range {
                start: Position {
                    line: diag.start_line - 1,
                    character: diag.start_col - 1,
                },
                end: Position {
                    line: diag.end_line - 1,
                    character: diag.end_col - 1,
                },
            },
            severity: Some(match diag.severity {
                DiagnosticSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
                DiagnosticSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
                DiagnosticSeverity::Hint => lsp_types::DiagnosticSeverity::HINT,
                DiagnosticSeverity::Info => lsp_types::DiagnosticSeverity::INFORMATION,
            }),
            code: diag.code.map(lsp_types::NumberOrString::String),
            message: diag.message,
            ..Default::default()
        }
    }
}
```

## Parsing Examples

### Error

Input:
```
src/Main.hs:15:10: error: [GHC-88464]
    Variable not in scope: foo :: Int -> String
    Suggested fix: Perhaps use `show` (imported from Prelude)
```

Output:
```rust
GhcDiagnostic {
    file: "src/Main.hs",
    start_line: 15,
    start_col: 10,
    severity: Error,
    code: Some("GHC-88464"),
    message: "Variable not in scope: foo :: Int -> String",
    notes: vec!["Suggested fix: Perhaps use `show` (imported from Prelude)"],
}
```

### Warning

Input:
```
src/Lib.hs:5:1: warning: [-Wunused-imports]
    The import of 'Data.Maybe' is redundant
```

Output:
```rust
GhcDiagnostic {
    file: "src/Lib.hs",
    start_line: 5,
    start_col: 1,
    severity: Warning,
    code: None,
    message: "The import of 'Data.Maybe' is redundant",
    notes: vec![],
}
```

## Best Practices

1. **Prefer JSON format** when GHC version supports it
2. **Fall back to text parsing** for older GHC
3. **Preserve error codes** for editor quick-fixes
4. **Include notes** for helpful suggestions
