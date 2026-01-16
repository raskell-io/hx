//! Error types for the plugin system.

use std::path::PathBuf;

/// Result type alias for plugin operations.
pub type Result<T> = std::result::Result<T, PluginError>;

/// Errors that can occur during plugin operations.
#[derive(Debug, thiserror::Error)]
pub enum PluginError {
    /// Failed to load a plugin file.
    #[error("failed to load plugin at {path}: {message}")]
    LoadError {
        path: PathBuf,
        message: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
    },

    /// Plugin execution failed at runtime.
    #[error("plugin runtime error in {plugin}: {message}")]
    RuntimeError {
        plugin: String,
        message: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
    },

    /// Plugin execution timed out.
    #[error("plugin {plugin} timed out during {event}")]
    Timeout { plugin: String, event: String },

    /// Error calling a plugin API function.
    #[error("API error in {function}: {message}")]
    ApiError { function: String, message: String },

    /// Plugin file not found.
    #[error("plugin not found: {path}")]
    NotFound { path: PathBuf },

    /// Invalid plugin configuration.
    #[error("plugin configuration error: {message}")]
    ConfigError {
        message: String,
        #[source]
        source: Option<Box<dyn std::error::Error + Send + Sync>>,
    },

    /// Command not found in registered plugins.
    #[error("unknown command: {name}")]
    UnknownCommand { name: String },

    /// I/O error during plugin operations.
    #[error("I/O error: {message}")]
    Io {
        message: String,
        #[source]
        source: std::io::Error,
    },
}

impl PluginError {
    /// Create a load error.
    pub fn load(path: impl Into<PathBuf>, message: impl Into<String>) -> Self {
        PluginError::LoadError {
            path: path.into(),
            message: message.into(),
            source: None,
        }
    }

    /// Create a load error with a source.
    pub fn load_with_source(
        path: impl Into<PathBuf>,
        message: impl Into<String>,
        source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        PluginError::LoadError {
            path: path.into(),
            message: message.into(),
            source: Some(Box::new(source)),
        }
    }

    /// Create a runtime error.
    pub fn runtime(plugin: impl Into<String>, message: impl Into<String>) -> Self {
        PluginError::RuntimeError {
            plugin: plugin.into(),
            message: message.into(),
            source: None,
        }
    }

    /// Create a timeout error.
    pub fn timeout(plugin: impl Into<String>, event: impl Into<String>) -> Self {
        PluginError::Timeout {
            plugin: plugin.into(),
            event: event.into(),
        }
    }

    /// Create an API error.
    pub fn api(function: impl Into<String>, message: impl Into<String>) -> Self {
        PluginError::ApiError {
            function: function.into(),
            message: message.into(),
        }
    }

    /// Create a not found error.
    pub fn not_found(path: impl Into<PathBuf>) -> Self {
        PluginError::NotFound { path: path.into() }
    }

    /// Create a config error.
    pub fn config(message: impl Into<String>) -> Self {
        PluginError::ConfigError {
            message: message.into(),
            source: None,
        }
    }

    /// Create an unknown command error.
    pub fn unknown_command(name: impl Into<String>) -> Self {
        PluginError::UnknownCommand { name: name.into() }
    }

    /// Create an I/O error.
    pub fn io(message: impl Into<String>, source: std::io::Error) -> Self {
        PluginError::Io {
            message: message.into(),
            source,
        }
    }

    /// Check if this error is recoverable (shouldn't stop the build).
    pub fn is_recoverable(&self) -> bool {
        matches!(
            self,
            PluginError::RuntimeError { .. } | PluginError::Timeout { .. }
        )
    }
}
