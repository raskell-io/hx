//! Diagnostic checks and fix suggestions for hx.
//!
//! This crate provides the `hx doctor` functionality to diagnose
//! toolchain and project issues.

use hx_config::MANIFEST_FILENAME;
use hx_core::error::Fix;
use hx_toolchain::{install::ghcup_install_command, Toolchain};
use hx_ui::{Output, Style};
use std::path::Path;

/// Severity of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Informational
    Info,
    /// Warning - something is suboptimal
    Warning,
    /// Error - something is broken
    Error,
}

/// A diagnostic check result.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Severity of the issue
    pub severity: Severity,
    /// Short description
    pub message: String,
    /// Suggested fixes
    pub fixes: Vec<Fix>,
}

impl Diagnostic {
    /// Create an error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            fixes: vec![],
        }
    }

    /// Create a warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            fixes: vec![],
        }
    }

    /// Create an info diagnostic.
    pub fn info(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Info,
            message: message.into(),
            fixes: vec![],
        }
    }

    /// Add a fix to this diagnostic.
    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fixes.push(fix);
        self
    }
}

/// Result of running doctor checks.
#[derive(Debug, Default)]
pub struct DoctorReport {
    /// All diagnostics found
    pub diagnostics: Vec<Diagnostic>,
}

impl DoctorReport {
    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }

    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Warning)
    }

    /// Add a diagnostic.
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Get the count of each severity.
    pub fn counts(&self) -> (usize, usize, usize) {
        let errors = self.diagnostics.iter().filter(|d| d.severity == Severity::Error).count();
        let warnings = self.diagnostics.iter().filter(|d| d.severity == Severity::Warning).count();
        let info = self.diagnostics.iter().filter(|d| d.severity == Severity::Info).count();
        (errors, warnings, info)
    }
}

/// Run all doctor checks.
pub async fn run_checks(project_dir: Option<&Path>) -> DoctorReport {
    let mut report = DoctorReport::default();

    // Detect toolchain
    let toolchain = Toolchain::detect().await;

    // Check ghcup
    check_ghcup(&toolchain, &mut report);

    // Check GHC
    check_ghc(&toolchain, &mut report);

    // Check Cabal
    check_cabal(&toolchain, &mut report);

    // Check HLS
    check_hls(&toolchain, &mut report);

    // Check project if we're in one
    if let Some(dir) = project_dir {
        check_project(dir, &mut report);
    }

    report
}

fn check_ghcup(toolchain: &Toolchain, report: &mut DoctorReport) {
    if !toolchain.ghcup.status.is_found() {
        report.add(
            Diagnostic::warning("ghcup not found - cannot manage toolchain versions")
                .with_fix(Fix::with_command(
                    "Install ghcup",
                    ghcup_install_command(),
                )),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "ghcup: {}",
            toolchain.ghcup.status.version().map(|v| v.to_string()).unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_ghc(toolchain: &Toolchain, report: &mut DoctorReport) {
    if !toolchain.ghc.status.is_found() {
        report.add(
            Diagnostic::error("ghc not found")
                .with_fix(Fix::with_command(
                    "Install GHC via ghcup",
                    "ghcup install ghc",
                ))
                .with_fix(Fix::with_command(
                    "Or use hx",
                    "hx toolchain install --ghc 9.8.2",
                )),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "ghc: {}",
            toolchain.ghc.status.version().map(|v| v.to_string()).unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_cabal(toolchain: &Toolchain, report: &mut DoctorReport) {
    if !toolchain.cabal.status.is_found() {
        report.add(
            Diagnostic::error("cabal not found")
                .with_fix(Fix::with_command(
                    "Install Cabal via ghcup",
                    "ghcup install cabal",
                )),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "cabal: {}",
            toolchain.cabal.status.version().map(|v| v.to_string()).unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_hls(toolchain: &Toolchain, report: &mut DoctorReport) {
    if !toolchain.hls.status.is_found() {
        report.add(
            Diagnostic::warning("haskell-language-server not found - IDE features unavailable")
                .with_fix(Fix::with_command(
                    "Install HLS via ghcup",
                    "ghcup install hls",
                )),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "hls: {}",
            toolchain.hls.status.version().map(|v| v.to_string()).unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_project(dir: &Path, report: &mut DoctorReport) {
    // Check for hx.toml
    let manifest_path = dir.join(MANIFEST_FILENAME);
    if !manifest_path.exists() {
        report.add(
            Diagnostic::warning(format!("{} not found", MANIFEST_FILENAME))
                .with_fix(Fix::with_command(
                    "Initialize hx project",
                    "hx init",
                )),
        );
    } else {
        report.add(Diagnostic::info(format!("{} found", MANIFEST_FILENAME)));
    }

    // Check for .cabal file
    let has_cabal = std::fs::read_dir(dir)
        .ok()
        .map(|entries| {
            entries
                .flatten()
                .any(|e| e.path().extension().is_some_and(|ext| ext == "cabal"))
        })
        .unwrap_or(false);

    if !has_cabal {
        report.add(
            Diagnostic::error("no .cabal file found")
                .with_fix(Fix::with_command(
                    "Initialize project",
                    "hx init",
                )),
        );
    } else {
        report.add(Diagnostic::info(".cabal file found"));
    }
}

/// Print the doctor report.
pub fn print_report(report: &DoctorReport, output: &Output) {
    output.header("Doctor Report");

    for diagnostic in &report.diagnostics {
        let prefix = match diagnostic.severity {
            Severity::Error => format!("{}", Style::error("✗")),
            Severity::Warning => format!("{}", Style::warning("⚠")),
            Severity::Info => format!("{}", Style::success("✓")),
        };

        eprintln!("  {} {}", prefix, diagnostic.message);

        for fix in &diagnostic.fixes {
            if let Some(ref cmd) = fix.command {
                eprintln!("    {} {}", Style::dim("fix:"), Style::command(cmd));
            } else {
                eprintln!("    {} {}", Style::dim("fix:"), fix.description);
            }
        }
    }

    let (errors, warnings, _) = report.counts();
    eprintln!();
    if errors > 0 {
        eprintln!(
            "{} {} error(s), {} warning(s)",
            Style::error("✗"),
            errors,
            warnings
        );
    } else if warnings > 0 {
        eprintln!(
            "{} {} warning(s)",
            Style::warning("⚠"),
            warnings
        );
    } else {
        eprintln!("{} All checks passed", Style::success("✓"));
    }
}
