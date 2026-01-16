//! Plugin execution context.
//!
//! Uses a thread-local pattern similar to Helix editor's Steel integration
//! for implicit context access during plugin execution.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

/// Build result information available to post-build hooks.
#[derive(Debug, Clone, Default)]
pub struct BuildContext {
    /// Whether the build succeeded.
    pub success: bool,
    /// Build duration.
    pub duration: Duration,
    /// Warning messages from the build.
    pub warnings: Vec<String>,
    /// Error messages from the build.
    pub errors: Vec<String>,
}

/// Test result information available to post-test hooks.
#[derive(Debug, Clone, Default)]
pub struct TestContext {
    /// Whether all tests passed.
    pub passed: bool,
    /// Number of tests passed.
    pub passed_count: usize,
    /// Number of tests failed.
    pub failed_count: usize,
    /// Number of tests skipped.
    pub skipped_count: usize,
    /// Test duration.
    pub duration: Duration,
}

/// Context available during plugin execution.
#[derive(Debug, Clone)]
pub struct PluginContext {
    /// Project root directory.
    pub project_root: PathBuf,

    /// Project name from hx.toml or cabal file.
    pub project_name: String,

    /// GHC version (if detected).
    pub ghc_version: Option<String>,

    /// Cabal file path (if found).
    pub cabal_file: Option<PathBuf>,

    /// Build context (for post-build hooks).
    pub build: Option<BuildContext>,

    /// Test context (for post-test hooks).
    pub test: Option<TestContext>,

    /// Environment variables to set for child processes.
    pub env_vars: HashMap<String, String>,

    /// Whether verbose output is enabled.
    pub verbose: bool,
}

impl PluginContext {
    /// Create a new plugin context.
    pub fn new(project_root: PathBuf, project_name: String) -> Self {
        PluginContext {
            project_root,
            project_name,
            ghc_version: None,
            cabal_file: None,
            build: None,
            test: None,
            env_vars: HashMap::new(),
            verbose: false,
        }
    }

    /// Set the GHC version.
    pub fn with_ghc_version(mut self, version: impl Into<String>) -> Self {
        self.ghc_version = Some(version.into());
        self
    }

    /// Set the cabal file path.
    pub fn with_cabal_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.cabal_file = Some(path.into());
        self
    }

    /// Set the build context.
    pub fn with_build_context(mut self, build: BuildContext) -> Self {
        self.build = Some(build);
        self
    }

    /// Set the test context.
    pub fn with_test_context(mut self, test: TestContext) -> Self {
        self.test = Some(test);
        self
    }

    /// Set verbose mode.
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Set an environment variable.
    pub fn set_env(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.env_vars.insert(key.into(), value.into());
    }
}

// Thread-local storage for the current plugin context.
// This allows API functions to access context without explicit parameters.
thread_local! {
    static CONTEXT: RefCell<Option<PluginContext>> = const { RefCell::new(None) };
}

/// Set the current plugin context for this thread.
pub fn set_context(ctx: PluginContext) {
    CONTEXT.with(|c| {
        *c.borrow_mut() = Some(ctx);
    });
}

/// Clear the current plugin context.
pub fn clear_context() {
    CONTEXT.with(|c| {
        *c.borrow_mut() = None;
    });
}

/// Access the current plugin context.
///
/// Returns None if no context is set (i.e., not running inside a plugin).
pub fn with_context<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&PluginContext) -> R,
{
    CONTEXT.with(|c| c.borrow().as_ref().map(f))
}

/// Access the current plugin context mutably.
pub fn with_context_mut<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&mut PluginContext) -> R,
{
    CONTEXT.with(|c| c.borrow_mut().as_mut().map(f))
}

/// Guard that sets context on creation and clears it on drop.
pub struct ContextGuard;

impl ContextGuard {
    /// Create a new context guard, setting the context.
    pub fn new(ctx: PluginContext) -> Self {
        set_context(ctx);
        ContextGuard
    }
}

impl Drop for ContextGuard {
    fn drop(&mut self) {
        clear_context();
    }
}
