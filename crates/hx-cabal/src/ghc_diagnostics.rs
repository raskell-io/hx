//! GHC compiler output parsing.
//!
//! This module provides parsers for GHC diagnostic output, supporting both:
//! - JSON format (GHC 9.0+ with `-fdiagnostics-json`)
//! - Text format (fallback for older GHC versions)

use hx_core::{
    DiagnosticReport, DiagnosticSeverity, GhcDiagnostic, QuickFix, SourceSpan, TextEdit,
};
use regex::Regex;
use serde::Deserialize;
use std::path::PathBuf;
use std::sync::OnceLock;
use tracing::debug;

// =============================================================================
// GHC JSON Format (GHC 9.0+)
// =============================================================================

/// GHC JSON diagnostic format.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GhcJsonDiagnostic {
    span: Option<GhcJsonSpan>,
    severity: String,
    message: GhcJsonMessage,
    #[serde(default)]
    hints: Vec<String>,
    #[serde(default)]
    code: Option<i32>,
    #[serde(default)]
    reason: Option<GhcJsonReason>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GhcJsonSpan {
    file: String,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
}

/// GHC message can be a simple string or an array of parts.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum GhcJsonMessage {
    Simple(String),
    Parts(Vec<GhcJsonMessagePart>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum GhcJsonMessagePart {
    Text(String),
    Tagged {
        #[serde(default)]
        text: String,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GhcJsonReason {
    #[serde(default)]
    flag: Option<String>,
}

/// Parse GHC JSON diagnostic output.
///
/// GHC 9.0+ supports `-fdiagnostics-json` which outputs one JSON object per line.
pub fn parse_ghc_json(output: &str) -> DiagnosticReport {
    let mut report = DiagnosticReport::new();

    for line in output.lines() {
        let line = line.trim();
        if line.is_empty() || !line.starts_with('{') {
            continue;
        }

        match serde_json::from_str::<GhcJsonDiagnostic>(line) {
            Ok(json_diag) => {
                let diag = convert_json_diagnostic(json_diag);
                report.add(diag);
            }
            Err(e) => {
                debug!("Failed to parse GHC JSON diagnostic: {}", e);
            }
        }
    }

    report
}

fn convert_json_diagnostic(json: GhcJsonDiagnostic) -> GhcDiagnostic {
    let span = json.span.map(|s| {
        SourceSpan::new(
            PathBuf::from(&s.file),
            s.start_line,
            s.start_col,
            s.end_line,
            s.end_col,
        )
    });

    let severity = match json.severity.to_lowercase().as_str() {
        "error" | "sorryerror" => DiagnosticSeverity::Error,
        "warning" => DiagnosticSeverity::Warning,
        _ => DiagnosticSeverity::Info,
    };

    let message = match json.message {
        GhcJsonMessage::Simple(s) => s,
        GhcJsonMessage::Parts(parts) => parts
            .into_iter()
            .map(|part| match part {
                GhcJsonMessagePart::Text(t) => t,
                GhcJsonMessagePart::Tagged { text } => text,
            })
            .collect::<Vec<_>>()
            .join(""),
    };

    let code = json.code.map(|c| format!("GHC-{:05}", c));
    let warning_flag = json.reason.and_then(|r| r.flag);

    let fixes = generate_quick_fixes(&message, &span, &warning_flag);

    GhcDiagnostic {
        span,
        severity,
        code,
        warning_flag,
        message,
        hints: json.hints,
        fixes,
    }
}

// =============================================================================
// GHC Text Format (Fallback)
// =============================================================================

/// Regex for parsing GHC text diagnostic lines.
/// Format: file:line:col[-endcol]: severity: [flag]? message?
fn diagnostic_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| {
        Regex::new(
            r"^(.+?):(\d+):(\d+)(?:-(\d+))?:\s*(error|warning|Error|Warning):\s*(?:\[(-W[^\]]+)\]\s*)?(.*)$"
        ).expect("Invalid regex pattern")
    })
}

/// Parse GHC text diagnostic output (fallback for older GHC versions).
pub fn parse_ghc_text(output: &str) -> DiagnosticReport {
    let mut report = DiagnosticReport::new();
    let mut current_diagnostic: Option<GhcDiagnostic> = None;
    let regex = diagnostic_regex();

    for line in output.lines() {
        // Try to parse as a new diagnostic line
        if let Some(caps) = regex.captures(line) {
            // Save previous diagnostic if any
            if let Some(mut prev) = current_diagnostic.take() {
                finalize_diagnostic(&mut prev);
                report.add(prev);
            }

            let file = PathBuf::from(&caps[1]);
            let start_line: u32 = caps[2].parse().unwrap_or(1);
            let start_col: u32 = caps[3].parse().unwrap_or(1);
            let end_col: u32 = caps
                .get(4)
                .and_then(|m| m.as_str().parse().ok())
                .unwrap_or(start_col + 1);

            let severity = match caps[5].to_lowercase().as_str() {
                "error" => DiagnosticSeverity::Error,
                "warning" => DiagnosticSeverity::Warning,
                _ => DiagnosticSeverity::Info,
            };

            let warning_flag = caps.get(6).map(|m| m.as_str().to_string());
            let message = caps[7].to_string();

            current_diagnostic = Some(GhcDiagnostic {
                span: Some(SourceSpan::new(
                    file, start_line, start_col, start_line, end_col,
                )),
                severity,
                code: None,
                warning_flag,
                message,
                hints: Vec::new(),
                fixes: Vec::new(),
            });
        } else if let Some(ref mut diag) = current_diagnostic {
            // Continuation line (indented)
            let trimmed = line.trim();
            if !trimmed.is_empty() && (line.starts_with(' ') || line.starts_with('\t')) {
                // If message is empty, first continuation becomes the message
                if diag.message.is_empty() {
                    diag.message = trimmed.to_string();
                } else {
                    diag.hints.push(trimmed.to_string());
                }
            }
        }
    }

    // Don't forget the last diagnostic
    if let Some(mut diag) = current_diagnostic {
        finalize_diagnostic(&mut diag);
        report.add(diag);
    }

    report
}

/// Finalize a diagnostic before adding to report.
fn finalize_diagnostic(diag: &mut GhcDiagnostic) {
    if diag.fixes.is_empty() {
        diag.fixes = generate_quick_fixes(&diag.message, &diag.span, &diag.warning_flag);
    }
}

// =============================================================================
// Quick Fix Generation
// =============================================================================

/// Generate quick fix suggestions based on error patterns.
fn generate_quick_fixes(
    message: &str,
    span: &Option<SourceSpan>,
    warning_flag: &Option<String>,
) -> Vec<QuickFix> {
    let mut fixes = Vec::new();

    // Pattern: "Variable not in scope: foo"
    if message.contains("Variable not in scope:") || message.contains("Not in scope:") {
        if let Some(var_name) = extract_identifier(message) {
            fixes.push(QuickFix::with_command(
                format!("Search for '{}'", var_name),
                format!("hx search {}", var_name),
            ));
        }
    }

    // Pattern: "Could not find module 'Foo'"
    if message.contains("Could not find module") || message.contains("Could not load module") {
        if let Some(module_name) = extract_quoted(message) {
            // Guess package name from module
            let package_guess = module_to_package(&module_name);
            fixes.push(QuickFix::with_command(
                format!("Add '{}' as a dependency", package_guess),
                format!("hx add {}", package_guess),
            ));
        }
    }

    // Pattern: "The import of '...' is redundant"
    if message.contains("is redundant") && message.contains("import") {
        if let Some(span) = span {
            fixes.push(
                QuickFix::with_edit(
                    "Remove redundant import".to_string(),
                    TextEdit::delete(span.clone()),
                )
                .preferred(),
            );
        }
    }

    // Pattern: "Defined but not used"
    if message.contains("Defined but not used:") {
        if let Some(var_name) = extract_identifier(message) {
            fixes.push(QuickFix::suggestion(format!(
                "Prefix with underscore: _{}",
                var_name
            )));
        }
        if let Some(flag) = warning_flag {
            let flag_name = flag.trim_start_matches("-W");
            fixes.push(QuickFix::suggestion(format!(
                "Add pragma: {{-# OPTIONS_GHC -Wno-{} #-}}",
                flag_name
            )));
        }
    }

    // Pattern: "Couldn't match type" or "Couldn't match expected type"
    if message.contains("Couldn't match") {
        fixes.push(QuickFix::suggestion(
            "Add type annotation to clarify".to_string(),
        ));
    }

    // Pattern: "No instance for"
    if message.contains("No instance for") {
        if let Some(constraint) = extract_parenthesized(message) {
            fixes.push(QuickFix::suggestion(format!(
                "Add constraint '{}' to type signature",
                constraint
            )));
        }
    }

    // Pattern: "Ambiguous type variable"
    if message.contains("ambiguous type variable") || message.contains("Ambiguous type variable") {
        fixes.push(QuickFix::suggestion(
            "Add type annotation to resolve ambiguity".to_string(),
        ));
    }

    // Pattern: "Pattern match(es) are non-exhaustive"
    if message.contains("non-exhaustive") && message.contains("pattern") {
        fixes.push(QuickFix::suggestion(
            "Add missing pattern cases".to_string(),
        ));
    }

    // Pattern: Parse error
    if message.contains("parse error") || message.contains("Parse error") {
        fixes.push(QuickFix::suggestion(
            "Check syntax near the reported location".to_string(),
        ));
    }

    // Pattern: "perhaps you meant" suggestions from GHC
    if message.contains("Perhaps you meant") || message.contains("perhaps you meant") {
        // GHC already provides suggestions in hints, no additional fix needed
    }

    fixes
}

/// Extract an identifier from a message like "Variable not in scope: foo".
fn extract_identifier(text: &str) -> Option<String> {
    // Try to extract from patterns like "Variable not in scope: foo" or "'foo'"
    if let Some(quoted) = extract_quoted(text) {
        return Some(quoted);
    }

    // Try to extract after ": " at the end
    if let Some(pos) = text.rfind(": ") {
        let rest = text[pos + 2..].trim();
        // Take until whitespace or special characters
        let ident: String = rest
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '\'')
            .collect();
        if !ident.is_empty() {
            return Some(ident);
        }
    }

    None
}

/// Extract text between single quotes.
fn extract_quoted(text: &str) -> Option<String> {
    let start = text.find('\'')?;
    let end = text[start + 1..].find('\'')? + start + 1;
    Some(text[start + 1..end].to_string())
}

/// Extract text between parentheses.
fn extract_parenthesized(text: &str) -> Option<String> {
    let start = text.find('(')?;
    let end = text.rfind(')')?;
    if end > start {
        Some(text[start + 1..end].to_string())
    } else {
        None
    }
}

/// Guess a package name from a module name.
fn module_to_package(module: &str) -> String {
    // Common mappings
    let mappings = [
        ("Data.Text", "text"),
        ("Data.ByteString", "bytestring"),
        ("Data.Aeson", "aeson"),
        ("Data.Vector", "vector"),
        ("Data.Map", "containers"),
        ("Data.Set", "containers"),
        ("Data.HashMap", "unordered-containers"),
        ("Data.HashSet", "unordered-containers"),
        ("Control.Lens", "lens"),
        ("Control.Monad.Trans", "transformers"),
        ("Control.Monad.State", "mtl"),
        ("Control.Monad.Reader", "mtl"),
        ("Control.Monad.Writer", "mtl"),
        ("Control.Monad.Except", "mtl"),
        ("Network.HTTP", "http-client"),
        ("System.FilePath", "filepath"),
        ("System.Directory", "directory"),
    ];

    for (prefix, package) in mappings {
        if module.starts_with(prefix) {
            return package.to_string();
        }
    }

    // Default: lowercase first component
    module.split('.').next().unwrap_or(module).to_lowercase()
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Check if a GHC version supports JSON diagnostics.
///
/// GHC 9.0+ supports `-fdiagnostics-json`.
pub fn supports_json_diagnostics(ghc_version: &str) -> bool {
    parse_ghc_version(ghc_version)
        .map(|(major, _)| major >= 9)
        .unwrap_or(false)
}

/// Parse a GHC version string like "9.8.2".
fn parse_ghc_version(version: &str) -> Option<(u32, u32)> {
    let parts: Vec<&str> = version.split('.').collect();
    let major = parts.first()?.parse().ok()?;
    let minor = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
    Some((major, minor))
}

/// Get the GHC flags needed for diagnostic output.
pub fn diagnostic_flags(ghc_version: &str) -> Vec<&'static str> {
    let mut flags = vec!["-fno-code", "-fdefer-type-errors"];

    if supports_json_diagnostics(ghc_version) {
        flags.push("-fdiagnostics-json");
    }

    flags
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ghc_version() {
        assert_eq!(parse_ghc_version("9.8.2"), Some((9, 8)));
        assert_eq!(parse_ghc_version("9.6.4"), Some((9, 6)));
        assert_eq!(parse_ghc_version("8.10.7"), Some((8, 10)));
        assert_eq!(parse_ghc_version("9"), Some((9, 0)));
    }

    #[test]
    fn test_supports_json_diagnostics() {
        assert!(supports_json_diagnostics("9.8.2"));
        assert!(supports_json_diagnostics("9.0.0"));
        assert!(!supports_json_diagnostics("8.10.7"));
    }

    #[test]
    fn test_extract_quoted() {
        assert_eq!(
            extract_quoted("Variable 'foo' not found"),
            Some("foo".to_string())
        );
        assert_eq!(extract_quoted("No quotes here"), None);
    }

    #[test]
    fn test_extract_identifier() {
        assert_eq!(
            extract_identifier("Variable not in scope: foo"),
            Some("foo".to_string())
        );
        assert_eq!(
            extract_identifier("Not in scope: 'bar'"),
            Some("bar".to_string())
        );
    }

    #[test]
    fn test_module_to_package() {
        assert_eq!(module_to_package("Data.Text"), "text");
        assert_eq!(module_to_package("Data.Map.Strict"), "containers");
        assert_eq!(module_to_package("Control.Monad.State"), "mtl");
        assert_eq!(module_to_package("MyModule.Foo"), "mymodule");
    }

    #[test]
    fn test_parse_ghc_text_error() {
        let output = r#"src/Main.hs:10:5: error:
    Variable not in scope: foo
    Perhaps you meant 'fooBar'"#;

        let report = parse_ghc_text(output);
        assert_eq!(report.error_count(), 1);

        let diag = report.iter().next().unwrap();
        assert_eq!(diag.severity, DiagnosticSeverity::Error);
        assert!(diag.message.contains("Variable not in scope"));
        assert_eq!(diag.span.as_ref().unwrap().start_line, 10);
        // First continuation line becomes the message, rest are hints
        assert_eq!(diag.hints.len(), 1);
        assert!(diag.hints[0].contains("Perhaps you meant"));
    }

    #[test]
    fn test_parse_ghc_text_warning() {
        let output = "src/Lib.hs:5:1: warning: [-Wunused-imports]\n    The import of 'Data.List' is redundant";

        let report = parse_ghc_text(output);
        assert_eq!(report.warning_count(), 1);

        let diag = report.iter().next().unwrap();
        assert_eq!(diag.severity, DiagnosticSeverity::Warning);
        assert_eq!(diag.warning_flag, Some("-Wunused-imports".to_string()));
    }

    #[test]
    fn test_parse_ghc_text_multiple() {
        let output = r#"src/A.hs:1:1: error: Parse error
src/B.hs:2:3: warning: Unused variable"#;

        let report = parse_ghc_text(output);
        assert_eq!(report.error_count(), 1);
        assert_eq!(report.warning_count(), 1);
        assert_eq!(report.files().count(), 2);
    }

    #[test]
    fn test_parse_ghc_json() {
        let output = r#"{"span":{"file":"src/Main.hs","startLine":10,"startCol":5,"endLine":10,"endCol":8},"severity":"error","message":"Variable not in scope: foo","hints":[],"code":88464}"#;

        let report = parse_ghc_json(output);
        assert_eq!(report.error_count(), 1);

        let diag = report.iter().next().unwrap();
        assert_eq!(diag.severity, DiagnosticSeverity::Error);
        assert_eq!(diag.code, Some("GHC-88464".to_string()));
    }

    #[test]
    fn test_quick_fixes_variable_not_in_scope() {
        let fixes = generate_quick_fixes("Variable not in scope: foo", &None, &None);
        assert!(!fixes.is_empty());
        assert!(fixes.iter().any(|f| {
            f.command
                .as_ref()
                .map(|c| c.contains("search"))
                .unwrap_or(false)
        }));
    }

    #[test]
    fn test_quick_fixes_module_not_found() {
        let fixes = generate_quick_fixes("Could not find module 'Data.Text'", &None, &None);
        assert!(!fixes.is_empty());
        assert!(fixes.iter().any(|f| {
            f.command
                .as_ref()
                .map(|c| c.contains("hx add"))
                .unwrap_or(false)
        }));
    }

    #[test]
    fn test_diagnostic_flags() {
        let flags_9 = diagnostic_flags("9.8.2");
        assert!(flags_9.contains(&"-fdiagnostics-json"));

        let flags_8 = diagnostic_flags("8.10.7");
        assert!(!flags_8.contains(&"-fdiagnostics-json"));
    }
}
