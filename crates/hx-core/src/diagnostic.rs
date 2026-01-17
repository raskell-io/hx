//! Structured diagnostics for GHC compiler output.
//!
//! This module provides types for representing GHC diagnostics with proper
//! source locations, enabling LSP integration and rich error display.

use crate::Fix;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// A source location span in a file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    /// File path
    pub file: PathBuf,
    /// Start line (1-indexed)
    pub start_line: u32,
    /// Start column (1-indexed)
    pub start_col: u32,
    /// End line (1-indexed)
    pub end_line: u32,
    /// End column (1-indexed)
    pub end_col: u32,
}

impl SourceSpan {
    /// Create a new source span.
    pub fn new(
        file: impl Into<PathBuf>,
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    ) -> Self {
        Self {
            file: file.into(),
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }

    /// Create a span from GHC-style point location (file:line:col).
    ///
    /// GHC often gives point locations rather than ranges, so this creates
    /// a minimal span at that point.
    pub fn from_point(file: impl Into<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            file: file.into(),
            start_line: line,
            start_col: col,
            end_line: line,
            end_col: col.saturating_add(1),
        }
    }

    /// Check if this span contains a position.
    pub fn contains(&self, line: u32, col: u32) -> bool {
        if line < self.start_line || line > self.end_line {
            return false;
        }
        if line == self.start_line && col < self.start_col {
            return false;
        }
        if line == self.end_line && col > self.end_col {
            return false;
        }
        true
    }

    /// Check if this span overlaps with another.
    pub fn overlaps(&self, other: &SourceSpan) -> bool {
        if self.file != other.file {
            return false;
        }
        // Check if ranges overlap
        !(self.end_line < other.start_line
            || (self.end_line == other.start_line && self.end_col < other.start_col)
            || other.end_line < self.start_line
            || (other.end_line == self.start_line && other.end_col < self.start_col))
    }
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.display(),
            self.start_line,
            self.start_col
        )
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticSeverity {
    /// Compilation error - prevents successful build
    Error,
    /// Warning - build succeeds but indicates potential issues
    Warning,
    /// Informational message
    Info,
    /// Hint or suggestion
    Hint,
}

impl DiagnosticSeverity {
    /// Check if this severity prevents a successful build.
    pub fn is_error(&self) -> bool {
        matches!(self, DiagnosticSeverity::Error)
    }
}

impl std::fmt::Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticSeverity::Error => write!(f, "error"),
            DiagnosticSeverity::Warning => write!(f, "warning"),
            DiagnosticSeverity::Info => write!(f, "info"),
            DiagnosticSeverity::Hint => write!(f, "hint"),
        }
    }
}

/// A text edit (replacement) for quick fixes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextEdit {
    /// The range to replace
    pub range: SourceSpan,
    /// New text to insert
    pub new_text: String,
}

impl TextEdit {
    /// Create a new text edit.
    pub fn new(range: SourceSpan, new_text: impl Into<String>) -> Self {
        Self {
            range,
            new_text: new_text.into(),
        }
    }

    /// Create an insertion at a point.
    pub fn insert(file: impl Into<PathBuf>, line: u32, col: u32, text: impl Into<String>) -> Self {
        Self {
            range: SourceSpan::from_point(file, line, col),
            new_text: text.into(),
        }
    }

    /// Create a deletion of a range.
    pub fn delete(range: SourceSpan) -> Self {
        Self {
            range,
            new_text: String::new(),
        }
    }
}

/// A quick fix suggestion for a diagnostic.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuickFix {
    /// Human-readable title for the fix
    pub title: String,
    /// Text edit to apply (if available)
    pub edit: Option<TextEdit>,
    /// Shell command alternative (if no direct edit)
    pub command: Option<String>,
    /// Whether this is the preferred fix
    pub is_preferred: bool,
}

impl QuickFix {
    /// Create a quick fix with a text edit.
    pub fn with_edit(title: impl Into<String>, edit: TextEdit) -> Self {
        Self {
            title: title.into(),
            edit: Some(edit),
            command: None,
            is_preferred: false,
        }
    }

    /// Create a quick fix with a command.
    pub fn with_command(title: impl Into<String>, command: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            edit: None,
            command: Some(command.into()),
            is_preferred: false,
        }
    }

    /// Create a quick fix with just a description.
    pub fn suggestion(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            edit: None,
            command: None,
            is_preferred: false,
        }
    }

    /// Mark this fix as preferred.
    pub fn preferred(mut self) -> Self {
        self.is_preferred = true;
        self
    }

    /// Convert to the error module's Fix type.
    pub fn to_fix(&self) -> Fix {
        if let Some(ref cmd) = self.command {
            Fix::with_command(&self.title, cmd)
        } else {
            Fix::new(&self.title)
        }
    }
}

/// A structured GHC diagnostic.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GhcDiagnostic {
    /// Location in source (None for general diagnostics)
    pub span: Option<SourceSpan>,
    /// Severity level
    pub severity: DiagnosticSeverity,
    /// GHC diagnostic code (e.g., "GHC-12345" for GHC 9.6+)
    pub code: Option<String>,
    /// Warning flag (e.g., "-Wunused-imports")
    pub warning_flag: Option<String>,
    /// Main diagnostic message
    pub message: String,
    /// Additional context/hints from GHC
    pub hints: Vec<String>,
    /// Quick fix suggestions
    pub fixes: Vec<QuickFix>,
}

impl GhcDiagnostic {
    /// Create a new error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            span: None,
            severity: DiagnosticSeverity::Error,
            code: None,
            warning_flag: None,
            message: message.into(),
            hints: Vec::new(),
            fixes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            span: None,
            severity: DiagnosticSeverity::Warning,
            code: None,
            warning_flag: None,
            message: message.into(),
            hints: Vec::new(),
            fixes: Vec::new(),
        }
    }

    /// Set the source span.
    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    /// Set the diagnostic code.
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set the warning flag.
    pub fn with_warning_flag(mut self, flag: impl Into<String>) -> Self {
        self.warning_flag = Some(flag.into());
        self
    }

    /// Add a hint.
    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hints.push(hint.into());
        self
    }

    /// Add a quick fix.
    pub fn with_fix(mut self, fix: QuickFix) -> Self {
        self.fixes.push(fix);
        self
    }

    /// Check if this is an error.
    pub fn is_error(&self) -> bool {
        self.severity.is_error()
    }

    /// Check if this diagnostic has the "unused" tag.
    pub fn is_unused(&self) -> bool {
        if let Some(ref flag) = self.warning_flag {
            flag.contains("unused") || flag.contains("redundant")
        } else {
            self.message.contains("not used")
                || self.message.contains("redundant")
                || self.message.contains("Defined but not used")
        }
    }

    /// Check if this diagnostic has the "deprecated" tag.
    pub fn is_deprecated(&self) -> bool {
        if let Some(ref flag) = self.warning_flag {
            flag.contains("deprecated")
        } else {
            self.message.contains("deprecated")
        }
    }

    /// Get the file path if available.
    pub fn file(&self) -> Option<&PathBuf> {
        self.span.as_ref().map(|s| &s.file)
    }

    /// Format the diagnostic for display.
    pub fn format(&self) -> String {
        let mut output = String::new();

        // Location prefix
        if let Some(ref span) = self.span {
            output.push_str(&format!("{}: ", span));
        }

        // Severity and code
        output.push_str(&format!("{}", self.severity));
        if let Some(ref code) = self.code {
            output.push_str(&format!(" [{}]", code));
        }
        if let Some(ref flag) = self.warning_flag {
            output.push_str(&format!(" [{}]", flag));
        }
        output.push_str(": ");

        // Message
        output.push_str(&self.message);

        // Hints
        for hint in &self.hints {
            output.push_str("\n    ");
            output.push_str(hint);
        }

        output
    }
}

impl std::fmt::Display for GhcDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// Collection of diagnostics from a build.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DiagnosticReport {
    /// Diagnostics grouped by file
    pub by_file: HashMap<PathBuf, Vec<GhcDiagnostic>>,
    /// Diagnostics without a file location
    pub general: Vec<GhcDiagnostic>,
}

impl DiagnosticReport {
    /// Create a new empty report.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a diagnostic to the report.
    pub fn add(&mut self, diagnostic: GhcDiagnostic) {
        if let Some(ref span) = diagnostic.span {
            self.by_file
                .entry(span.file.clone())
                .or_default()
                .push(diagnostic);
        } else {
            self.general.push(diagnostic);
        }
    }

    /// Add multiple diagnostics.
    pub fn extend(&mut self, diagnostics: impl IntoIterator<Item = GhcDiagnostic>) {
        for diag in diagnostics {
            self.add(diag);
        }
    }

    /// Merge another report into this one.
    pub fn merge(&mut self, other: DiagnosticReport) {
        for (file, diagnostics) in other.by_file {
            self.by_file.entry(file).or_default().extend(diagnostics);
        }
        self.general.extend(other.general);
    }

    /// Get all diagnostics for a file.
    pub fn for_file(&self, file: &PathBuf) -> Option<&Vec<GhcDiagnostic>> {
        self.by_file.get(file)
    }

    /// Iterate over all diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &GhcDiagnostic> {
        self.by_file.values().flatten().chain(self.general.iter())
    }

    /// Count of error diagnostics.
    pub fn error_count(&self) -> usize {
        self.iter().filter(|d| d.is_error()).count()
    }

    /// Count of warning diagnostics.
    pub fn warning_count(&self) -> usize {
        self.iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .count()
    }

    /// Total count of all diagnostics.
    pub fn total_count(&self) -> usize {
        self.by_file.values().map(|v| v.len()).sum::<usize>() + self.general.len()
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.iter().any(|d| d.is_error())
    }

    /// Check if the report is empty.
    pub fn is_empty(&self) -> bool {
        self.by_file.is_empty() && self.general.is_empty()
    }

    /// Get all files with diagnostics.
    pub fn files(&self) -> impl Iterator<Item = &PathBuf> {
        self.by_file.keys()
    }

    /// Clear all diagnostics for a file.
    pub fn clear_file(&mut self, file: &PathBuf) {
        self.by_file.remove(file);
    }

    /// Clear all diagnostics.
    pub fn clear(&mut self) {
        self.by_file.clear();
        self.general.clear();
    }

    /// Convert errors to Vec<String> for compatibility.
    pub fn errors_as_strings(&self) -> Vec<String> {
        self.iter()
            .filter(|d| d.is_error())
            .map(|d| d.format())
            .collect()
    }

    /// Convert warnings to Vec<String> for compatibility.
    pub fn warnings_as_strings(&self) -> Vec<String> {
        self.iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .map(|d| d.format())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_span_from_point() {
        let span = SourceSpan::from_point("src/Main.hs", 10, 5);
        assert_eq!(span.start_line, 10);
        assert_eq!(span.start_col, 5);
        assert_eq!(span.end_line, 10);
        assert_eq!(span.end_col, 6);
    }

    #[test]
    fn test_source_span_contains() {
        let span = SourceSpan::new("test.hs", 10, 5, 10, 15);
        assert!(span.contains(10, 5));
        assert!(span.contains(10, 10));
        assert!(span.contains(10, 15));
        assert!(!span.contains(10, 4));
        assert!(!span.contains(10, 16));
        assert!(!span.contains(9, 10));
        assert!(!span.contains(11, 10));
    }

    #[test]
    fn test_source_span_overlaps() {
        let span1 = SourceSpan::new("test.hs", 10, 5, 10, 15);
        let span2 = SourceSpan::new("test.hs", 10, 10, 10, 20);
        let span3 = SourceSpan::new("test.hs", 10, 20, 10, 30);
        let span4 = SourceSpan::new("other.hs", 10, 5, 10, 15);

        assert!(span1.overlaps(&span2));
        assert!(!span1.overlaps(&span3));
        assert!(!span1.overlaps(&span4)); // Different file
    }

    #[test]
    fn test_diagnostic_severity_display() {
        assert_eq!(format!("{}", DiagnosticSeverity::Error), "error");
        assert_eq!(format!("{}", DiagnosticSeverity::Warning), "warning");
    }

    #[test]
    fn test_ghc_diagnostic_builder() {
        let diag = GhcDiagnostic::error("Variable not in scope: foo")
            .with_span(SourceSpan::from_point("src/Main.hs", 10, 5))
            .with_code("GHC-88464")
            .with_hint("Perhaps you meant 'fooBar'");

        assert!(diag.is_error());
        assert_eq!(diag.code, Some("GHC-88464".to_string()));
        assert_eq!(diag.hints.len(), 1);
    }

    #[test]
    fn test_diagnostic_report() {
        let mut report = DiagnosticReport::new();

        report.add(GhcDiagnostic::error("Error 1").with_span(SourceSpan::from_point("a.hs", 1, 1)));
        report.add(
            GhcDiagnostic::warning("Warning 1").with_span(SourceSpan::from_point("a.hs", 2, 1)),
        );
        report.add(GhcDiagnostic::error("Error 2").with_span(SourceSpan::from_point("b.hs", 1, 1)));
        report.add(GhcDiagnostic::error("General error"));

        assert_eq!(report.error_count(), 3);
        assert_eq!(report.warning_count(), 1);
        assert_eq!(report.total_count(), 4);
        assert!(report.has_errors());
        assert_eq!(report.files().count(), 2);
    }

    #[test]
    fn test_quick_fix() {
        let fix = QuickFix::with_command("Add import", "hx add text").preferred();
        assert!(fix.is_preferred);
        assert_eq!(fix.command, Some("hx add text".to_string()));

        let core_fix = fix.to_fix();
        assert_eq!(core_fix.command, Some("hx add text".to_string()));
    }

    #[test]
    fn test_diagnostic_is_unused() {
        let diag1 = GhcDiagnostic::warning("Defined but not used: 'x'");
        assert!(diag1.is_unused());

        let diag2 =
            GhcDiagnostic::warning("Import is redundant").with_warning_flag("-Wunused-imports");
        assert!(diag2.is_unused());

        let diag3 = GhcDiagnostic::error("Type mismatch");
        assert!(!diag3.is_unused());
    }
}
