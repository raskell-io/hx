# hx-plugins

Plugin system with lifecycle hooks for hx.

## Overview

`hx-plugins` provides:

- **Lifecycle hooks** - Pre/post build, test, run, etc.
- **Shell scripts** - Simple script execution
- **Steel plugins** - Lisp-based scripted extensions
- **Custom commands** - User-defined CLI commands

## Quick Start

### Using Hooks in hx.toml

```toml
[plugins]
enabled = true
hook_timeout_ms = 5000
continue_on_error = false

[plugins.hooks]
pre_build = ["./scripts/lint.sh"]
post_build = ["./scripts/notify.sh"]
post_test = ["./scripts/coverage.sh"]
```

### Programmatic Usage

```rust
use hx_plugins::{PluginManager, PluginConfig, HookEvent, PluginContext};

// Create manager
let config = PluginConfig::from_manifest(&manifest);
let mut manager = PluginManager::new(config)?;

// Initialize and load plugins
manager.initialize()?;
manager.load_all(&project_root)?;

// Run hooks
let ctx = PluginContext::new(project_root, project_name);
let result = manager.run_hook(HookEvent::PreBuild, &ctx)?;

if !result.success {
    eprintln!("Pre-build hook failed: {:?}", result.error);
}
```

## Hook Events

| Event | When | Context |
|-------|------|---------|
| `PreBuild` | Before `hx build` | - |
| `PostBuild` | After build | BuildContext |
| `PreTest` | Before `hx test` | - |
| `PostTest` | After tests | TestContext |
| `PreRun` | Before `hx run` | - |
| `PostRun` | After run | - |
| `PreClean` | Before `hx clean` | - |
| `PostClean` | After clean | - |
| `PreLock` | Before `hx lock` | - |
| `PostLock` | After lock | - |
| `Init` | After `hx init` | - |

## HookEvent

```rust
pub enum HookEvent {
    PreBuild,
    PostBuild,
    PreTest,
    PostTest,
    PreRun,
    PostRun,
    PreClean,
    PostClean,
    PreLock,
    PostLock,
    Init,
}

impl HookEvent {
    /// Get config key name (e.g., "pre_build")
    pub fn config_key(&self) -> &'static str;
}
```

## Context Types

### PluginContext

```rust
pub struct PluginContext {
    /// Project root directory
    pub project_root: PathBuf,

    /// Project name
    pub project_name: String,

    /// GHC version (if known)
    pub ghc_version: Option<String>,

    /// Build context (for post-build)
    pub build: Option<BuildContext>,

    /// Test context (for post-test)
    pub test: Option<TestContext>,
}
```

### BuildContext

```rust
pub struct BuildContext {
    /// Build succeeded
    pub success: bool,

    /// Build duration
    pub duration: Duration,

    /// Warning messages
    pub warnings: Vec<String>,

    /// Error messages
    pub errors: Vec<String>,
}
```

### TestContext

```rust
pub struct TestContext {
    /// All tests passed
    pub passed: bool,

    /// Number of passed tests
    pub passed_count: usize,

    /// Number of failed tests
    pub failed_count: usize,

    /// Number of skipped tests
    pub skipped_count: usize,

    /// Test duration
    pub duration: Duration,
}
```

## HookResult

```rust
pub struct HookResult {
    /// Hook succeeded
    pub success: bool,

    /// Execution duration
    pub duration: Duration,

    /// Error message (if failed)
    pub error: Option<String>,

    /// Hook output
    pub output: String,
}
```

## Configuration

### PluginConfig

```rust
pub struct PluginConfig {
    /// Enable plugin system
    pub enabled: bool,

    /// Hook execution timeout (ms)
    pub hook_timeout_ms: u64,

    /// Plugin search paths
    pub paths: Vec<PathBuf>,

    /// Continue on hook failure
    pub continue_on_error: bool,

    /// Hook scripts
    pub scripts: PluginHookConfig,
}
```

### From Manifest

```rust
impl From<ManifestPlugins> for PluginConfig {
    fn from(manifest: ManifestPlugins) -> Self;
}
```

## Shell Script Hooks

Simple shell scripts for common tasks:

```bash
#!/bin/bash
# scripts/pre-build.sh

echo "Running pre-build checks..."

# Lint check
hlint src/

# Format check
fourmolu --mode check src/

exit $?
```

Configure in hx.toml:

```toml
[plugins.hooks]
pre_build = ["./scripts/pre-build.sh"]
```

## Environment Variables

Hooks receive environment variables:

| Variable | Value |
|----------|-------|
| `HX_PROJECT_ROOT` | Project directory |
| `HX_PROJECT_NAME` | Project name |
| `HX_GHC_VERSION` | GHC version |
| `HX_HOOK_EVENT` | Hook event name |
| `HX_BUILD_SUCCESS` | "true"/"false" (post-build) |
| `HX_TEST_PASSED` | "true"/"false" (post-test) |

## Plugin Manager

```rust
pub struct PluginManager {
    config: PluginConfig,
    engine: Option<SteelEngine>,
    initialized: bool,
}

impl PluginManager {
    /// Create with config
    pub fn new(config: PluginConfig) -> Result<Self>;

    /// Create with defaults
    pub fn with_defaults() -> Result<Self>;

    /// Get config reference
    pub fn config(&self) -> &PluginConfig;

    /// Initialize engine
    pub fn initialize(&mut self) -> Result<()>;

    /// Load all plugins from paths
    pub fn load_all(&mut self, root: &Path) -> Result<()>;

    /// Load single plugin
    pub fn load_plugin(&mut self, path: &Path) -> Result<()>;

    /// Run hook
    pub fn run_hook(&mut self, event: HookEvent, ctx: &PluginContext) -> Result<HookResult>;
}
```

## Error Handling

```rust
pub enum PluginError {
    /// Plugin file not found
    NotFound(PathBuf),

    /// Plugin load failed
    LoadError(String),

    /// Hook execution failed
    ExecutionError(String),

    /// Hook timed out
    Timeout(Duration),

    /// Plugin system not initialized
    NotInitialized,
}
```

## CLI Integration (PluginHooks Helper)

For command implementations:

```rust
use crate::plugins::PluginHooks;

// In build command
let mut hooks = PluginHooks::from_project(&project, ghc_version);
if let Some(ref mut h) = hooks {
    h.initialize()?;
}

// Pre-build
if let Some(ref mut h) = hooks {
    if !h.run_pre_hook(HookEvent::PreBuild, output) {
        return Ok(ExitCode::HookError.into());
    }
}

// ... run build ...

// Post-build
if let Some(ref mut h) = hooks {
    h.run_post_build_hook(success, duration, warnings, errors, output);
}
```

## Best Practices

1. **Keep hooks fast** - Don't block builds unnecessarily
2. **Use timeouts** - Set reasonable `hook_timeout_ms`
3. **Fail early** - Use `continue_on_error = false` for CI
4. **Idempotent scripts** - Hooks may run multiple times
5. **Exit codes matter** - Non-zero = failure
