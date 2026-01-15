//! Diagnostic checks and fix suggestions for hx.
//!
//! This crate provides the `hx doctor` functionality to diagnose
//! toolchain and project issues.

use hx_config::MANIFEST_FILENAME;
use hx_core::error::Fix;
use hx_toolchain::{Toolchain, install::ghcup_install_command};
use hx_ui::{Output, Style};
use std::path::Path;
use std::process::Command;

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
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Warning)
    }

    /// Add a diagnostic.
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Get the count of each severity.
    pub fn counts(&self) -> (usize, usize, usize) {
        let errors = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count();
        let warnings = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count();
        let info = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Info)
            .count();
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

    // Check native dependencies (platform-specific)
    check_native_deps(&mut report);

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
                .with_fix(Fix::with_command("Install ghcup", ghcup_install_command())),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "ghcup: {}",
            toolchain
                .ghcup
                .status
                .version()
                .map(|v| v.to_string())
                .unwrap_or_else(|| "installed".to_string())
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
            toolchain
                .ghc
                .status
                .version()
                .map(|v| v.to_string())
                .unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_cabal(toolchain: &Toolchain, report: &mut DoctorReport) {
    if !toolchain.cabal.status.is_found() {
        report.add(
            Diagnostic::error("cabal not found").with_fix(Fix::with_command(
                "Install Cabal via ghcup",
                "ghcup install cabal",
            )),
        );
    } else {
        report.add(Diagnostic::info(format!(
            "cabal: {}",
            toolchain
                .cabal
                .status
                .version()
                .map(|v| v.to_string())
                .unwrap_or_else(|| "installed".to_string())
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
            toolchain
                .hls
                .status
                .version()
                .map(|v| v.to_string())
                .unwrap_or_else(|| "installed".to_string())
        )));
    }
}

fn check_native_deps(report: &mut DoctorReport) {
    #[cfg(target_os = "linux")]
    check_native_deps_linux(report);

    #[cfg(target_os = "macos")]
    check_native_deps_macos(report);

    #[cfg(target_os = "windows")]
    check_native_deps_windows(report);
}

#[cfg(target_os = "linux")]
fn check_native_deps_linux(report: &mut DoctorReport) {
    // Check for common native libraries needed by GHC
    let libs = [
        (
            "libgmp",
            "libgmp-dev",
            "GMP (GNU Multiple Precision Arithmetic Library)",
        ),
        ("libz", "zlib1g-dev", "zlib compression library"),
        ("libncurses", "libncurses-dev", "ncurses terminal library"),
        ("libffi", "libffi-dev", "Foreign Function Interface library"),
    ];

    for (lib, package, description) in libs {
        if !check_library_linux(lib) {
            report.add(
                Diagnostic::warning(format!(
                    "{} not found - {} may fail to build",
                    lib, description
                ))
                .with_fix(Fix::with_command(
                    format!("Install {} (Debian/Ubuntu)", package),
                    format!("sudo apt-get install {}", package),
                ))
                .with_fix(Fix::with_command(
                    "Install on Fedora/RHEL",
                    format!("sudo dnf install {}-devel", lib.trim_start_matches("lib")),
                )),
            );
        }
    }
}

#[cfg(target_os = "linux")]
fn check_library_linux(lib: &str) -> bool {
    // Try pkg-config first
    if let Ok(output) = Command::new("pkg-config").args(["--exists", lib]).output() {
        if output.status.success() {
            return true;
        }
    }

    // Try ldconfig as fallback
    if let Ok(output) = Command::new("ldconfig").args(["-p"]).output() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if stdout.contains(lib) {
            return true;
        }
    }

    // Check common library paths
    let lib_paths = [
        format!("/usr/lib/x86_64-linux-gnu/{}.so", lib),
        format!("/usr/lib/{}.so", lib),
        format!("/lib/x86_64-linux-gnu/{}.so", lib),
    ];

    for path in lib_paths {
        if std::path::Path::new(&path).exists() {
            return true;
        }
    }

    false
}

#[cfg(target_os = "macos")]
fn check_native_deps_macos(report: &mut DoctorReport) {
    // Check if Homebrew is available
    let has_brew = Command::new("brew").arg("--version").output().is_ok();

    if !has_brew {
        report.add(
            Diagnostic::warning("Homebrew not found - recommended for installing native dependencies")
                .with_fix(Fix::with_command(
                    "Install Homebrew",
                    "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"",
                )),
        );
        return;
    }

    // Check for common dependencies
    let deps = [
        ("gmp", "GMP arithmetic library"),
        ("zlib", "zlib compression"),
    ];

    for (formula, description) in deps {
        if !check_brew_formula(formula) {
            report.add(
                Diagnostic::warning(format!(
                    "{} not found - {} may fail to build",
                    formula, description
                ))
                .with_fix(Fix::with_command(
                    format!("Install {}", formula),
                    format!("brew install {}", formula),
                )),
            );
        }
    }
}

#[cfg(target_os = "macos")]
fn check_brew_formula(formula: &str) -> bool {
    Command::new("brew")
        .args(["list", formula])
        .output()
        .is_ok_and(|o| o.status.success())
}

#[cfg(target_os = "windows")]
fn check_native_deps_windows(report: &mut DoctorReport) {
    // Check for MSYS2/MinGW which provides native libs on Windows
    let msys2_paths = ["C:\\msys64", "C:\\msys32"];

    let has_msys2 = msys2_paths.iter().any(|p| std::path::Path::new(p).exists());

    if !has_msys2 {
        report.add(
            Diagnostic::warning("MSYS2 not found - some packages may fail to build")
                .with_fix(Fix::new("Install MSYS2 from https://www.msys2.org/")),
        );
    }
}

fn check_project(dir: &Path, report: &mut DoctorReport) {
    // Check for hx.toml
    let manifest_path = dir.join(MANIFEST_FILENAME);
    if !manifest_path.exists() {
        report.add(
            Diagnostic::warning(format!("{} not found", MANIFEST_FILENAME))
                .with_fix(Fix::with_command("Initialize hx project", "hx init")),
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
                .with_fix(Fix::with_command("Initialize project", "hx init")),
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
        eprintln!("{} {} warning(s)", Style::warning("⚠"), warnings);
    } else {
        eprintln!("{} All checks passed", Style::success("✓"));
    }
}
