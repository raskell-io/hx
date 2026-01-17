//! BHC build options and utilities.

use hx_config::BhcProfile;
use std::path::PathBuf;

/// BHC-specific build options.
#[derive(Debug, Clone, Default)]
pub struct BhcBuildOptions {
    /// Optimization profile.
    pub profile: BhcProfile,
    /// Emit kernel performance report.
    pub emit_kernel_report: bool,
    /// Enable tensor fusion optimizations.
    pub tensor_fusion: bool,
    /// Cross-compilation target.
    pub target: Option<String>,
    /// Output directory for build artifacts.
    pub output_dir: Option<PathBuf>,
    /// Number of parallel jobs.
    pub jobs: Option<usize>,
    /// Enable verbose output.
    pub verbose: bool,
}

impl BhcBuildOptions {
    /// Create new build options with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the optimization profile.
    pub fn with_profile(mut self, profile: BhcProfile) -> Self {
        self.profile = profile;
        self
    }

    /// Enable kernel report emission.
    pub fn with_kernel_report(mut self, enabled: bool) -> Self {
        self.emit_kernel_report = enabled;
        self
    }

    /// Enable tensor fusion.
    pub fn with_tensor_fusion(mut self, enabled: bool) -> Self {
        self.tensor_fusion = enabled;
        self
    }

    /// Set the cross-compilation target.
    pub fn with_target(mut self, target: impl Into<String>) -> Self {
        self.target = Some(target.into());
        self
    }

    /// Set the output directory.
    pub fn with_output_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.output_dir = Some(dir.into());
        self
    }

    /// Set the number of parallel jobs.
    pub fn with_jobs(mut self, jobs: usize) -> Self {
        self.jobs = Some(jobs);
        self
    }

    /// Enable verbose output.
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }
}
