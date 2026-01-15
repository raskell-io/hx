//! Error types for hx.

use std::path::PathBuf;

/// Result type alias using hx Error.
pub type Result<T> = std::result::Result<T, Error>;

/// Error codes for categorizing failures.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    /// Tool not found in PATH
    ToolchainMissing,
    /// Wrong version of a tool
    ToolchainMismatch,
    /// HLS version doesn't match GHC
    HlsMismatch,
    /// Native library not found
    SystemDepMissing,
    /// Package resolution failed
    SolverFailure,
    /// Compilation error
    BuildFailure,
    /// Invalid configuration
    ConfigError,
    /// I/O error
    IoError,
    /// Command execution failed
    CommandFailed,
    /// Lock file error
    LockError,
}

/// A fix suggestion for an error.
#[derive(Debug, Clone)]
pub struct Fix {
    /// Description of what this fix does
    pub description: String,
    /// Command to run, if applicable
    pub command: Option<String>,
}

impl Fix {
    /// Create a fix with just a description.
    pub fn new(description: impl Into<String>) -> Self {
        Self {
            description: description.into(),
            command: None,
        }
    }

    /// Create a fix with a command.
    pub fn with_command(description: impl Into<String>, command: impl Into<String>) -> Self {
        Self {
            description: description.into(),
            command: Some(command.into()),
        }
    }
}

/// Structured error type for hx.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("toolchain not found: {tool}")]
    ToolchainMissing {
        tool: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    #[error("toolchain version mismatch for {tool}: expected {expected}, found {found}")]
    ToolchainMismatch {
        tool: String,
        expected: String,
        found: String,
        fixes: Vec<Fix>,
    },

    #[error("configuration error: {message}")]
    Config {
        message: String,
        path: Option<PathBuf>,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    #[error("I/O error: {message}")]
    Io {
        message: String,
        path: Option<PathBuf>,
        #[source]
        source: std::io::Error,
    },

    #[error("command failed: {command}")]
    CommandFailed {
        command: String,
        exit_code: Option<i32>,
        stdout: String,
        stderr: String,
        fixes: Vec<Fix>,
    },

    #[error("build failed")]
    BuildFailed {
        errors: Vec<String>,
        fixes: Vec<Fix>,
    },

    #[error("lock error: {message}")]
    Lock {
        message: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
        fixes: Vec<Fix>,
    },

    #[error("project not found")]
    ProjectNotFound {
        searched: Vec<PathBuf>,
        fixes: Vec<Fix>,
    },

    #[error("{0}")]
    Other(#[from] anyhow::Error),
}

impl Error {
    /// Get the error code for this error.
    pub fn code(&self) -> ErrorCode {
        match self {
            Error::ToolchainMissing { .. } => ErrorCode::ToolchainMissing,
            Error::ToolchainMismatch { .. } => ErrorCode::ToolchainMismatch,
            Error::Config { .. } => ErrorCode::ConfigError,
            Error::Io { .. } => ErrorCode::IoError,
            Error::CommandFailed { .. } => ErrorCode::CommandFailed,
            Error::BuildFailed { .. } => ErrorCode::BuildFailure,
            Error::Lock { .. } => ErrorCode::LockError,
            Error::ProjectNotFound { .. } => ErrorCode::ConfigError,
            Error::Other(_) => ErrorCode::IoError,
        }
    }

    /// Get suggested fixes for this error.
    pub fn fixes(&self) -> &[Fix] {
        match self {
            Error::ToolchainMissing { fixes, .. } => fixes,
            Error::ToolchainMismatch { fixes, .. } => fixes,
            Error::Config { fixes, .. } => fixes,
            Error::CommandFailed { fixes, .. } => fixes,
            Error::BuildFailed { fixes, .. } => fixes,
            Error::Lock { fixes, .. } => fixes,
            Error::ProjectNotFound { fixes, .. } => fixes,
            Error::Io { .. } | Error::Other(_) => &[],
        }
    }

    /// Create a config error.
    pub fn config(message: impl Into<String>) -> Self {
        Error::Config {
            message: message.into(),
            path: None,
            source: None,
            fixes: vec![],
        }
    }

    /// Create a config error with a path.
    pub fn config_at(message: impl Into<String>, path: impl Into<PathBuf>) -> Self {
        Error::Config {
            message: message.into(),
            path: Some(path.into()),
            source: None,
            fixes: vec![],
        }
    }
}
