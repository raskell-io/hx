//! Hook events and dispatch for the plugin system.

use std::fmt;
use std::time::Duration;

/// Events that can trigger plugin hooks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HookEvent {
    /// Before build starts
    PreBuild,
    /// After build completes
    PostBuild,
    /// Before tests run
    PreTest,
    /// After tests complete
    PostTest,
    /// Before run command
    PreRun,
    /// After run completes
    PostRun,
    /// Before clean
    PreClean,
    /// After clean completes
    PostClean,
    /// Before lock generation
    PreLock,
    /// After lock completes
    PostLock,
    /// Project initialization
    Init,
}

impl HookEvent {
    /// Get all hook events.
    pub fn all() -> &'static [HookEvent] {
        &[
            HookEvent::PreBuild,
            HookEvent::PostBuild,
            HookEvent::PreTest,
            HookEvent::PostTest,
            HookEvent::PreRun,
            HookEvent::PostRun,
            HookEvent::PreClean,
            HookEvent::PostClean,
            HookEvent::PreLock,
            HookEvent::PostLock,
            HookEvent::Init,
        ]
    }

    /// Get the hook name as used in configuration.
    pub fn config_key(&self) -> &'static str {
        match self {
            HookEvent::PreBuild => "pre_build",
            HookEvent::PostBuild => "post_build",
            HookEvent::PreTest => "pre_test",
            HookEvent::PostTest => "post_test",
            HookEvent::PreRun => "pre_run",
            HookEvent::PostRun => "post_run",
            HookEvent::PreClean => "pre_clean",
            HookEvent::PostClean => "post_clean",
            HookEvent::PreLock => "pre_lock",
            HookEvent::PostLock => "post_lock",
            HookEvent::Init => "init",
        }
    }

    /// Get the Scheme function name for this hook.
    pub fn scheme_function(&self) -> &'static str {
        match self {
            HookEvent::PreBuild => "pre-build-hook",
            HookEvent::PostBuild => "post-build-hook",
            HookEvent::PreTest => "pre-test-hook",
            HookEvent::PostTest => "post-test-hook",
            HookEvent::PreRun => "pre-run-hook",
            HookEvent::PostRun => "post-run-hook",
            HookEvent::PreClean => "pre-clean-hook",
            HookEvent::PostClean => "post-clean-hook",
            HookEvent::PreLock => "pre-lock-hook",
            HookEvent::PostLock => "post-lock-hook",
            HookEvent::Init => "init-hook",
        }
    }

    /// Check if this is a "pre" event.
    pub fn is_pre(&self) -> bool {
        matches!(
            self,
            HookEvent::PreBuild
                | HookEvent::PreTest
                | HookEvent::PreRun
                | HookEvent::PreClean
                | HookEvent::PreLock
        )
    }

    /// Check if this is a "post" event.
    pub fn is_post(&self) -> bool {
        matches!(
            self,
            HookEvent::PostBuild
                | HookEvent::PostTest
                | HookEvent::PostRun
                | HookEvent::PostClean
                | HookEvent::PostLock
        )
    }
}

impl fmt::Display for HookEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.config_key())
    }
}

/// Result of running a hook.
#[derive(Debug, Clone)]
pub struct HookResult {
    /// Whether the hook succeeded.
    pub success: bool,
    /// How long the hook took to run.
    pub duration: Duration,
    /// Output from the hook (if any).
    pub output: Option<String>,
    /// Error message if failed.
    pub error: Option<String>,
}

impl HookResult {
    /// Create a successful result.
    pub fn success(duration: Duration) -> Self {
        HookResult {
            success: true,
            duration,
            output: None,
            error: None,
        }
    }

    /// Create a successful result with output.
    pub fn success_with_output(duration: Duration, output: String) -> Self {
        HookResult {
            success: true,
            duration,
            output: Some(output),
            error: None,
        }
    }

    /// Create a failed result.
    pub fn failure(duration: Duration, error: String) -> Self {
        HookResult {
            success: false,
            duration,
            output: None,
            error: Some(error),
        }
    }

    /// Create a skipped result (hook not defined).
    pub fn skipped() -> Self {
        HookResult {
            success: true,
            duration: Duration::ZERO,
            output: None,
            error: None,
        }
    }
}
