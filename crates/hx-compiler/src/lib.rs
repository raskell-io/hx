//! Compiler backend abstraction for hx.
//!
//! This crate provides a trait-based abstraction over different Haskell
//! compilers, enabling hx to support multiple backends like GHC and BHC.

pub mod registry;

use async_trait::async_trait;
use hx_ui::Output;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::time::Duration;
use thiserror::Error;

/// Error type for compiler operations.
#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("compiler not found: {name}")]
    NotFound { name: String },

    #[error("compiler version mismatch: expected {expected}, found {found}")]
    VersionMismatch { expected: String, found: String },

    #[error("build failed with {error_count} error(s)")]
    BuildFailed {
        error_count: usize,
        errors: Vec<String>,
    },

    #[error("check failed with {error_count} error(s)")]
    CheckFailed {
        error_count: usize,
        errors: Vec<String>,
    },

    #[error("run failed: {message}")]
    RunFailed { message: String },

    #[error("unsupported target: {target}")]
    UnsupportedTarget { target: String },

    #[error("compiler error: {message}")]
    Other { message: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for compiler operations.
pub type Result<T> = std::result::Result<T, CompilerError>;

/// Status of a compiler installation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompilerStatus {
    /// Compiler is installed and ready.
    Available {
        /// Version string.
        version: String,
        /// Path to the compiler executable.
        path: PathBuf,
    },
    /// Compiler is not installed.
    NotInstalled,
    /// Compiler version does not match requirements.
    VersionMismatch {
        /// Required version.
        required: String,
        /// Installed version.
        installed: String,
    },
}

impl CompilerStatus {
    /// Check if the compiler is available.
    pub fn is_available(&self) -> bool {
        matches!(self, CompilerStatus::Available { .. })
    }

    /// Get the version if available.
    pub fn version(&self) -> Option<&str> {
        match self {
            CompilerStatus::Available { version, .. } => Some(version),
            CompilerStatus::VersionMismatch { installed, .. } => Some(installed),
            CompilerStatus::NotInstalled => None,
        }
    }

    /// Get the path if available.
    pub fn path(&self) -> Option<&Path> {
        match self {
            CompilerStatus::Available { path, .. } => Some(path),
            _ => None,
        }
    }
}

/// Options for building a project.
#[derive(Debug, Clone, Default)]
pub struct BuildOptions {
    /// Build in release mode with optimizations.
    pub release: bool,
    /// Optimization level (0, 1, or 2).
    pub optimization: Option<u8>,
    /// Number of parallel jobs.
    pub jobs: Option<usize>,
    /// Target triple for cross-compilation.
    pub target: Option<String>,
    /// Specific package to build (for workspaces).
    pub package: Option<String>,
    /// Enable verbose output.
    pub verbose: bool,
    /// Additional compiler-specific flags.
    pub extra_flags: Vec<String>,
    /// Source directories.
    pub src_dirs: Vec<PathBuf>,
    /// Treat warnings as errors.
    pub werror: bool,
}

/// Result of a build operation.
#[derive(Debug, Clone)]
pub struct BuildResult {
    /// Whether the build succeeded.
    pub success: bool,
    /// Build duration.
    pub duration: Duration,
    /// Number of modules compiled.
    pub modules_compiled: usize,
    /// Number of modules skipped (up-to-date).
    pub modules_skipped: usize,
    /// Path to the output executable (if binary project).
    pub executable: Option<PathBuf>,
    /// Path to the output library (if library project).
    pub library: Option<PathBuf>,
    /// Any warnings encountered.
    pub warnings: Vec<Diagnostic>,
    /// Any errors encountered.
    pub errors: Vec<Diagnostic>,
}

impl Default for BuildResult {
    fn default() -> Self {
        Self {
            success: false,
            duration: Duration::ZERO,
            modules_compiled: 0,
            modules_skipped: 0,
            executable: None,
            library: None,
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }
}

/// Options for type-checking a project.
#[derive(Debug, Clone, Default)]
pub struct CheckOptions {
    /// Specific package to check (for workspaces).
    pub package: Option<String>,
    /// Enable verbose output.
    pub verbose: bool,
    /// Additional compiler-specific flags.
    pub extra_flags: Vec<String>,
}

/// Result of a check operation.
#[derive(Debug, Clone)]
pub struct CheckResult {
    /// Whether the check succeeded.
    pub success: bool,
    /// Check duration.
    pub duration: Duration,
    /// Number of modules checked.
    pub modules_checked: usize,
    /// Any warnings encountered.
    pub warnings: Vec<Diagnostic>,
    /// Any errors encountered.
    pub errors: Vec<Diagnostic>,
}

impl Default for CheckResult {
    fn default() -> Self {
        Self {
            success: false,
            duration: Duration::ZERO,
            modules_checked: 0,
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }
}

/// Options for running a project.
#[derive(Debug, Clone, Default)]
pub struct RunOptions {
    /// Arguments to pass to the program.
    pub args: Vec<String>,
    /// Specific package to run (for workspaces).
    pub package: Option<String>,
    /// Target triple (for cross-compiled binaries).
    pub target: Option<String>,
    /// Enable verbose output.
    pub verbose: bool,
}

/// Result of a run operation.
#[derive(Debug, Clone)]
pub struct RunResult {
    /// Exit code from the program.
    pub exit_code: i32,
    /// Run duration.
    pub duration: Duration,
}

impl Default for RunResult {
    fn default() -> Self {
        Self {
            exit_code: 0,
            duration: Duration::ZERO,
        }
    }
}

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    /// Error - compilation cannot continue.
    Error,
    /// Warning - potential issue.
    Warning,
    /// Hint - suggestion for improvement.
    Hint,
}

/// A diagnostic message from the compiler.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    /// Severity level.
    pub severity: DiagnosticSeverity,
    /// Error/warning code (if available).
    pub code: Option<String>,
    /// The diagnostic message.
    pub message: String,
    /// Source file path.
    pub file: Option<PathBuf>,
    /// Line number (1-indexed).
    pub line: Option<u32>,
    /// Column number (1-indexed).
    pub column: Option<u32>,
    /// Additional notes or suggestions.
    pub notes: Vec<String>,
}

impl Diagnostic {
    /// Create a new error diagnostic.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Error,
            code: None,
            message: message.into(),
            file: None,
            line: None,
            column: None,
            notes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic.
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Warning,
            code: None,
            message: message.into(),
            file: None,
            line: None,
            column: None,
            notes: Vec::new(),
        }
    }

    /// Add a source location to this diagnostic.
    pub fn with_location(mut self, file: PathBuf, line: u32, column: u32) -> Self {
        self.file = Some(file);
        self.line = Some(line);
        self.column = Some(column);
        self
    }

    /// Add an error code to this diagnostic.
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Add a note to this diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let severity = match self.severity {
            DiagnosticSeverity::Error => "error",
            DiagnosticSeverity::Warning => "warning",
            DiagnosticSeverity::Hint => "hint",
        };

        if let Some(ref file) = self.file {
            write!(f, "{}:", file.display())?;
            if let Some(line) = self.line {
                write!(f, "{}:", line)?;
                if let Some(col) = self.column {
                    write!(f, "{}:", col)?;
                }
            }
            write!(f, " ")?;
        }

        if let Some(ref code) = self.code {
            write!(f, "{}[{}]: {}", severity, code, self.message)
        } else {
            write!(f, "{}: {}", severity, self.message)
        }
    }
}

/// The main trait for compiler backends.
///
/// Implementations of this trait provide the build, check, and run
/// functionality for a specific compiler (e.g., GHC, BHC).
#[async_trait]
pub trait CompilerBackend: Send + Sync {
    /// Get the name of this compiler backend.
    fn name(&self) -> &str;

    /// Get a human-readable description of this backend.
    fn description(&self) -> &str;

    /// Detect the compiler installation status.
    async fn detect(&self) -> Result<CompilerStatus>;

    /// Get the version string of the installed compiler.
    async fn version(&self) -> Result<String>;

    /// Build the project.
    ///
    /// # Arguments
    /// * `project_root` - Root directory of the project
    /// * `options` - Build configuration options
    /// * `output` - UI output handler
    async fn build(
        &self,
        project_root: &Path,
        options: &BuildOptions,
        output: &Output,
    ) -> Result<BuildResult>;

    /// Type-check the project without producing output.
    ///
    /// # Arguments
    /// * `project_root` - Root directory of the project
    /// * `options` - Check configuration options
    /// * `output` - UI output handler
    async fn check(
        &self,
        project_root: &Path,
        options: &CheckOptions,
        output: &Output,
    ) -> Result<CheckResult>;

    /// Run the project.
    ///
    /// # Arguments
    /// * `project_root` - Root directory of the project
    /// * `options` - Run configuration options
    /// * `output` - UI output handler
    async fn run(
        &self,
        project_root: &Path,
        options: &RunOptions,
        output: &Output,
    ) -> Result<RunResult>;

    /// Parse raw compiler output into structured diagnostics.
    fn parse_diagnostics(&self, raw_output: &str) -> Vec<Diagnostic>;

    /// Get the list of supported target triples for cross-compilation.
    fn supported_targets(&self) -> Vec<&str> {
        vec![]
    }

    /// Check if a specific target is supported.
    fn supports_target(&self, target: &str) -> bool {
        self.supported_targets().contains(&target)
    }
}

pub use registry::CompilerRegistry;
