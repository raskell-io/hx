//! Plugin system for hx using Steel Scheme.
//!
//! This crate provides:
//! - Pre/post build hooks
//! - Custom commands via Scheme scripts
//! - A Steel API for interacting with hx
//!
//! # Example
//!
//! ```ignore
//! use hx_plugins::{PluginManager, PluginConfig};
//!
//! let config = PluginConfig::default();
//! let mut manager = PluginManager::new(config)?;
//! manager.initialize()?;
//!
//! // Run pre-build hooks
//! let ctx = PluginContext::new(project_root, project_name);
//! manager.run_hook(HookEvent::PreBuild, &ctx)?;
//! ```

pub mod api;
pub mod commands;
pub mod config;
pub mod context;
pub mod engine;
pub mod error;
pub mod hooks;
pub mod loader;

// Re-exports for convenience
pub use commands::CustomCommand;
pub use config::{HookConfig, PluginConfig};
pub use context::{BuildContext, PluginContext, TestContext};
pub use engine::{PluginSystem, SteelEngine};
pub use error::{PluginError, Result};
pub use hooks::{HookEvent, HookResult};
pub use loader::{discover_plugins, find_plugin, DiscoveredPlugin, PluginPaths};

use std::path::Path;

/// Main plugin manager that handles all plugin operations.
pub struct PluginManager {
    engine: SteelEngine,
    initialized: bool,
}

impl PluginManager {
    /// Create a new plugin manager with the given configuration.
    pub fn new(config: PluginConfig) -> Result<Self> {
        Ok(PluginManager {
            engine: SteelEngine::new(config),
            initialized: false,
        })
    }

    /// Create a new plugin manager with default configuration.
    pub fn with_defaults() -> Result<Self> {
        Self::new(PluginConfig::new())
    }

    /// Initialize the plugin system.
    ///
    /// This registers all API functions and loads the prelude.
    pub fn initialize(&mut self) -> Result<()> {
        if self.initialized {
            return Ok(());
        }

        self.engine.initialize()?;
        self.initialized = true;
        Ok(())
    }

    /// Load all plugins from the configured paths.
    pub fn load_all(&mut self, project_root: &Path) -> Result<()> {
        self.ensure_initialized()?;

        let plugins = discover_plugins(self.engine.config(), project_root)?;

        for plugin in plugins {
            if let Err(e) = self.engine.load_plugin(&plugin.path) {
                // Log but don't fail on individual plugin errors
                tracing::warn!("Failed to load plugin {}: {}", plugin.name, e);
            }
        }

        Ok(())
    }

    /// Load a specific plugin by name or path.
    pub fn load_plugin(&mut self, path: &Path) -> Result<()> {
        self.ensure_initialized()?;
        self.engine.load_plugin(path)
    }

    /// Run hooks for an event.
    pub fn run_hook(&mut self, event: HookEvent, ctx: &PluginContext) -> Result<HookResult> {
        self.ensure_initialized()?;
        self.engine.run_hook(event, ctx)
    }

    /// Run a custom command.
    pub fn run_command(&mut self, name: &str, args: &[String]) -> Result<i32> {
        self.ensure_initialized()?;
        self.engine.run_command(name, args)
    }

    /// Get the list of registered custom commands.
    pub fn commands(&self) -> &std::collections::HashMap<String, CustomCommand> {
        self.engine.commands()
    }

    /// Get the plugin configuration.
    pub fn config(&self) -> &PluginConfig {
        self.engine.config()
    }

    /// Check if a command is registered.
    pub fn has_command(&self, name: &str) -> bool {
        self.engine.commands().contains_key(name)
    }

    /// Ensure the manager is initialized.
    fn ensure_initialized(&mut self) -> Result<()> {
        if !self.initialized {
            self.initialize()?;
        }
        Ok(())
    }
}

/// Check if plugins are enabled in the given configuration.
pub fn plugins_enabled(config: &PluginConfig) -> bool {
    config.enabled
}

/// Create a simple plugin context for testing.
pub fn test_context(project_root: impl Into<std::path::PathBuf>, name: &str) -> PluginContext {
    PluginContext::new(project_root.into(), name.to_string())
}
