//! Full BHC native build orchestrator.
//!
//! Builds all dependencies from source, then the local project. This module
//! ties together the package build pipeline and the local project builder,
//! processing packages in topological order from a resolved build plan.

use crate::native::{BhcCompilerConfig, BhcNativeBuildOptions, BhcNativeError};
use crate::native_builder::{BhcNativeBuildResult, BhcNativeBuilder};
use crate::package_db::BhcPackageDb;
use hx_solver::plan::BuildPlan;
use hx_ui::Output;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tracing::{debug, info, warn};

/// Full BHC native build orchestrator.
///
/// Handles the complete build pipeline: resolve dependencies from build plan,
/// build each dependency in topological order, register in package database,
/// then build the local project.
pub struct BhcFullNativeBuilder {
    /// BHC compiler configuration.
    bhc: BhcCompilerConfig,
    /// Package database for registering built packages.
    ///
    /// Will be used when the dependency build loop is implemented.
    #[allow(dead_code)]
    package_db: BhcPackageDb,
    /// Root directory for cached build artifacts.
    ///
    /// Will be used when the dependency build loop is implemented.
    #[allow(dead_code)]
    cache_dir: PathBuf,
    /// Map of package name to computed package ID for already-built packages.
    built_packages: HashMap<String, String>,
}

impl BhcFullNativeBuilder {
    /// Create a new full native builder.
    ///
    /// The `cache_dir` is used to store built package artifacts for reuse
    /// across builds.
    pub fn new(bhc: BhcCompilerConfig, package_db: BhcPackageDb, cache_dir: PathBuf) -> Self {
        Self {
            bhc,
            package_db,
            cache_dir,
            built_packages: HashMap::new(),
        }
    }

    /// Build the full project with all dependencies.
    ///
    /// Processes the build plan in topological order:
    /// 1. Pre-installed packages are skipped.
    /// 2. Source packages would be built and registered (future work).
    /// 3. The local project is built using `BhcNativeBuilder`.
    ///
    /// Note: the actual per-package source extraction and compilation loop
    /// requires integration with hx-solver's fetch/extract infrastructure,
    /// which is future work. Currently this skeleton focuses on building
    /// the local project with whatever packages are already registered.
    pub async fn build_project(
        &mut self,
        project_root: &Path,
        plan: &BuildPlan,
        options: &BhcNativeBuildOptions,
        output: &Output,
    ) -> Result<BhcNativeBuildResult, BhcNativeError> {
        info!("Building project with BHC native pipeline");
        info!("Build plan has {} packages", plan.packages.len());

        // Track pre-installed vs source packages
        let mut pre_installed = 0;
        let mut source_packages = 0;
        for unit in &plan.packages {
            if unit.pre_installed {
                pre_installed += 1;
            } else {
                source_packages += 1;
            }
        }
        debug!(
            "{} pre-installed, {} to build from source",
            pre_installed, source_packages
        );

        // TODO: For each non-pre-installed package in topological order:
        //   1. Check if already cached (by package ID)
        //   2. If not cached, fetch source tarball via hx-solver fetch
        //   3. Extract tarball
        //   4. Build package using BhcPackageBuildConfig
        //   5. Register in package_db
        //   6. Record in built_packages map
        //
        // This requires integration with hx-solver's fetch/extract APIs
        // and is tracked as future work.

        if source_packages > 0 {
            warn!(
                "{} packages need building from source; \
                 dependency build loop not yet implemented, \
                 proceeding with local project only",
                source_packages
            );
        }

        // Build local project
        let builder = BhcNativeBuilder::new(BhcCompilerConfig {
            bhc_path: self.bhc.bhc_path.clone(),
            version: self.bhc.version.clone(),
            profile: self.bhc.profile,
            package_dbs: self.bhc.package_dbs.clone(),
            packages: self.bhc.packages.clone(),
            tensor_fusion: self.bhc.tensor_fusion,
            emit_kernel_report: self.bhc.emit_kernel_report,
        });
        builder.build(project_root, options, output).await
    }

    /// Get the map of built package name to package ID.
    pub fn built_packages(&self) -> &HashMap<String, String> {
        &self.built_packages
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hx_config::BhcProfile;

    fn test_config() -> BhcCompilerConfig {
        BhcCompilerConfig {
            bhc_path: PathBuf::from("bhc"),
            version: "2026.2.0".to_string(),
            profile: BhcProfile::default(),
            package_dbs: Vec::new(),
            packages: Vec::new(),
            tensor_fusion: false,
            emit_kernel_report: false,
        }
    }

    #[test]
    fn test_full_native_builder_new() {
        let config = test_config();
        let package_db = BhcPackageDb {
            path: PathBuf::from("/tmp/test.db"),
            bhc_version: "2026.2.0".to_string(),
            bhc_pkg_path: PathBuf::from("bhc-pkg"),
            registered: std::collections::HashSet::new(),
        };
        let cache_dir = PathBuf::from("/tmp/cache");

        let builder = BhcFullNativeBuilder::new(config, package_db, cache_dir.clone());

        assert!(builder.built_packages().is_empty());
        assert_eq!(builder.cache_dir, cache_dir);
        assert_eq!(builder.bhc.version, "2026.2.0");
    }
}
