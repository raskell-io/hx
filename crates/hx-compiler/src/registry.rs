//! Compiler backend registry.
//!
//! Manages the available compiler backends and provides lookup functionality.

use crate::{CompilerBackend, CompilerError, Result};
use std::collections::HashMap;
use std::sync::Arc;

/// Registry for compiler backends.
///
/// The registry maintains a collection of available compiler backends
/// and provides methods to look them up by name.
pub struct CompilerRegistry {
    backends: HashMap<String, Arc<dyn CompilerBackend>>,
    default_backend: Option<String>,
}

impl CompilerRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            backends: HashMap::new(),
            default_backend: None,
        }
    }

    /// Register a compiler backend.
    pub fn register(&mut self, backend: Arc<dyn CompilerBackend>) {
        let name = backend.name().to_string();
        self.backends.insert(name, backend);
    }

    /// Set the default backend.
    pub fn set_default(&mut self, name: impl Into<String>) {
        self.default_backend = Some(name.into());
    }

    /// Get a backend by name.
    pub fn get(&self, name: &str) -> Option<Arc<dyn CompilerBackend>> {
        self.backends.get(name).cloned()
    }

    /// Get the default backend.
    pub fn default(&self) -> Option<Arc<dyn CompilerBackend>> {
        self.default_backend
            .as_ref()
            .and_then(|name| self.backends.get(name).cloned())
    }

    /// Get a backend by name, or return an error if not found.
    pub fn get_or_err(&self, name: &str) -> Result<Arc<dyn CompilerBackend>> {
        self.get(name).ok_or_else(|| CompilerError::NotFound {
            name: name.to_string(),
        })
    }

    /// Get the default backend, or return an error if none is set.
    pub fn default_or_err(&self) -> Result<Arc<dyn CompilerBackend>> {
        self.default().ok_or_else(|| CompilerError::Other {
            message: "no default compiler backend configured".to_string(),
        })
    }

    /// List all registered backend names.
    pub fn list(&self) -> Vec<&str> {
        self.backends.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a backend is registered.
    pub fn has(&self, name: &str) -> bool {
        self.backends.contains_key(name)
    }

    /// Get the number of registered backends.
    pub fn len(&self) -> usize {
        self.backends.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.backends.is_empty()
    }
}

impl Default for CompilerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        BuildOptions, BuildResult, CheckOptions, CheckResult, CompilerStatus, Diagnostic,
        RunOptions, RunResult,
    };
    use async_trait::async_trait;
    use std::path::Path;

    struct MockBackend {
        name: String,
    }

    #[async_trait]
    impl CompilerBackend for MockBackend {
        fn name(&self) -> &str {
            &self.name
        }

        fn description(&self) -> &str {
            "Mock compiler backend for testing"
        }

        async fn detect(&self) -> Result<CompilerStatus> {
            Ok(CompilerStatus::NotInstalled)
        }

        async fn version(&self) -> Result<String> {
            Ok("1.0.0".to_string())
        }

        async fn build(
            &self,
            _project_root: &Path,
            _options: &BuildOptions,
            _output: &hx_ui::Output,
        ) -> Result<BuildResult> {
            Ok(BuildResult::default())
        }

        async fn check(
            &self,
            _project_root: &Path,
            _options: &CheckOptions,
            _output: &hx_ui::Output,
        ) -> Result<CheckResult> {
            Ok(CheckResult::default())
        }

        async fn run(
            &self,
            _project_root: &Path,
            _options: &RunOptions,
            _output: &hx_ui::Output,
        ) -> Result<RunResult> {
            Ok(RunResult::default())
        }

        fn parse_diagnostics(&self, _raw_output: &str) -> Vec<Diagnostic> {
            Vec::new()
        }
    }

    #[test]
    fn test_registry_basic() {
        let mut registry = CompilerRegistry::new();
        assert!(registry.is_empty());

        let backend = Arc::new(MockBackend {
            name: "test".to_string(),
        });
        registry.register(backend);

        assert!(!registry.is_empty());
        assert_eq!(registry.len(), 1);
        assert!(registry.has("test"));
        assert!(!registry.has("other"));
    }

    #[test]
    fn test_registry_default() {
        let mut registry = CompilerRegistry::new();

        let ghc = Arc::new(MockBackend {
            name: "ghc".to_string(),
        });
        let bhc = Arc::new(MockBackend {
            name: "bhc".to_string(),
        });

        registry.register(ghc);
        registry.register(bhc);
        registry.set_default("ghc");

        assert!(registry.default().is_some());
        assert_eq!(registry.default().unwrap().name(), "ghc");
    }
}
