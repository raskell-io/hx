//! Build command implementation.

use anyhow::Result;
use hx_cabal::build::{self as cabal_build, BuildOptions};
use hx_cache::{BuildState, StoreIndex, compute_source_fingerprint, save_source_fingerprint};
use hx_config::{Project, find_project_root};
use hx_lock::Lockfile;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::Output;
use std::time::Instant;

/// Run the build command.
pub async fn run(
    release: bool,
    jobs: Option<usize>,
    target: Option<String>,
    package: Option<String>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Check toolchain requirements and get toolchain info
    let toolchain = Toolchain::detect().await;
    if let Err(e) = ensure_toolchain(
        &toolchain,
        project.manifest.toolchain.ghc.as_deref(),
        project.manifest.toolchain.cabal.as_deref(),
        policy,
    )
    .await
    {
        output.print_error(&e);
        return Ok(4); // Toolchain error exit code
    }

    // Validate package selection for workspaces
    if let Some(ref pkg_name) = package {
        if project.is_workspace() {
            if project.get_package(pkg_name).is_none() {
                output.error(&format!("Package '{}' not found in workspace", pkg_name));
                output.info(&format!(
                    "Available packages: {}",
                    project.package_names().join(", ")
                ));
                return Ok(1);
            }
        } else {
            output.warn("--package flag ignored (not a workspace project)");
        }
    }

    let build_target = match &package {
        Some(pkg) if project.is_workspace() => pkg.as_str(),
        _ => project.name(),
    };

    // Check lockfile for fingerprint
    let (lock_fingerprint, package_count) = get_build_fingerprint(&project);

    // Compute source fingerprint for incremental build detection
    let source_fingerprint = match compute_source_fingerprint(&project.root) {
        Ok(fp) => Some(fp),
        Err(e) => {
            output.verbose(&format!("Could not compute source fingerprint: {}", e));
            None
        }
    };

    // Check if we can skip the build entirely
    if let Some(ref source_fp) = source_fingerprint {
        let build_state = BuildState::load(&project.root).unwrap_or_default();
        if build_state.is_fresh(&source_fp.hash, lock_fingerprint.as_deref()) {
            output.status("Fresh", build_target);
            output.info("No changes detected, skipping build");
            return Ok(0);
        }
    }

    // Check if we have cached dependencies
    if let Some(ref fp) = lock_fingerprint
        && let Ok(store) = StoreIndex::load()
        && store.has_cached_build(fp)
    {
        output.verbose("Dependencies cached, build should be fast");
    }

    if project.is_workspace() {
        output.status(
            "Building",
            &format!(
                "{} ({}/{} packages)",
                build_target,
                if package.is_some() {
                    1
                } else {
                    project.package_count()
                },
                project.package_count()
            ),
        );
    } else {
        output.status("Building", build_target);
    }

    let ghc_version = toolchain.ghc.status.version().map(|v| v.to_string());

    let options = BuildOptions {
        release,
        jobs,
        target,
        package: package.clone(),
        verbose: output.is_verbose(),
        fingerprint: lock_fingerprint.clone(),
        ghc_version: ghc_version.clone(),
        package_count: Some(package_count),
        project_name: Some(project.name().to_string()),
    };

    let build_dir = project.cabal_build_dir();
    let build_start = Instant::now();

    match cabal_build::build(&project.root, &build_dir, &options, output).await {
        Ok(_result) => {
            // Update build state on success
            if let Some(ref source_fp) = source_fingerprint {
                let mut build_state = BuildState::load(&project.root).unwrap_or_default();
                build_state.update_fingerprints(
                    &source_fp.hash,
                    lock_fingerprint.as_deref(),
                    ghc_version.as_deref(),
                );
                build_state.mark_success(
                    build_target,
                    &source_fp.hash,
                    build_start.elapsed().as_secs_f64(),
                );
                if let Err(e) = build_state.save(&project.root) {
                    output.verbose(&format!("Could not save build state: {}", e));
                }
                if let Err(e) = save_source_fingerprint(&project.root, source_fp) {
                    output.verbose(&format!("Could not save source fingerprint: {}", e));
                }
            }
            Ok(0)
        }
        Err(e) => {
            // Update build state on failure
            if source_fingerprint.is_some() {
                let mut build_state = BuildState::load(&project.root).unwrap_or_default();
                build_state.mark_failed(build_target, &e.to_string());
                let _ = build_state.save(&project.root);
            }
            output.print_error(&e);
            Ok(5) // Build error exit code
        }
    }
}

/// Get build fingerprint from lockfile if it exists.
fn get_build_fingerprint(project: &Project) -> (Option<String>, usize) {
    let lockfile_path = project.lockfile_path();
    if !lockfile_path.exists() {
        return (None, 0);
    }

    match Lockfile::from_file(&lockfile_path) {
        Ok(lock) => {
            let fingerprint = lock.plan.hash.clone();
            let package_count = lock.packages.len();
            (fingerprint, package_count)
        }
        Err(_) => (None, 0),
    }
}

/// Run the test command.
pub async fn test(
    pattern: Option<String>,
    package: Option<String>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Check toolchain requirements
    if let Err(e) = check_toolchain(&project, policy).await {
        output.print_error(&e);
        return Ok(4); // Toolchain error exit code
    }

    // Validate package selection for workspaces
    if let Some(ref pkg_name) = package {
        if project.is_workspace() {
            if project.get_package(pkg_name).is_none() {
                output.error(&format!("Package '{}' not found in workspace", pkg_name));
                output.info(&format!(
                    "Available packages: {}",
                    project.package_names().join(", ")
                ));
                return Ok(1);
            }
        } else {
            output.warn("--package flag ignored (not a workspace project)");
        }
    }

    let test_target = match &package {
        Some(pkg) if project.is_workspace() => pkg.as_str(),
        _ => project.name(),
    };

    output.status("Testing", test_target);

    let build_dir = project.cabal_build_dir();

    match cabal_build::test(
        &project.root,
        &build_dir,
        pattern.as_deref(),
        package.as_deref(),
        output,
    )
    .await
    {
        Ok(_result) => Ok(0),
        Err(e) => {
            output.print_error(&e);
            Ok(5) // Test error exit code
        }
    }
}

/// Check toolchain requirements and install if needed.
async fn check_toolchain(project: &Project, policy: AutoInstallPolicy) -> hx_core::Result<()> {
    let toolchain = Toolchain::detect().await;

    ensure_toolchain(
        &toolchain,
        project.manifest.toolchain.ghc.as_deref(),
        project.manifest.toolchain.cabal.as_deref(),
        policy,
    )
    .await
}
