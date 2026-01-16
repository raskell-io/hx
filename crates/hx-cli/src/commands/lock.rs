//! Lock command implementation.

use anyhow::Result;
use hx_cabal::freeze;
use hx_config::{LOCKFILE_FILENAME, Project, find_project_root};
use hx_lock::{Lockfile, WorkspacePackageInfo, parse_freeze_file};
use hx_toolchain::Toolchain;
use hx_ui::{Output, Spinner};
use std::path::Path;

/// Run the lock command.
pub async fn run(output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    if project.is_workspace() {
        output.status(
            "Locking",
            &format!("{} ({} packages)", project.name(), project.package_count()),
        );
    } else {
        output.status("Locking", project.name());
    }

    // Update package index
    freeze::update(output.is_verbose()).await?;

    // Run cabal freeze
    let build_dir = project.cabal_build_dir();
    let freeze_content = freeze::freeze(&project.root, &build_dir).await?;

    // Create lockfile
    let spinner = Spinner::new("Creating lockfile...");

    let mut lockfile = Lockfile::new();

    // Set toolchain versions from detected tools
    let toolchain = Toolchain::detect().await;
    lockfile.set_toolchain(
        toolchain.ghc.status.version().map(|v| v.to_string()),
        toolchain.cabal.status.version().map(|v| v.to_string()),
    );

    // Set workspace info if this is a workspace
    if project.is_workspace() {
        let workspace_packages = collect_workspace_packages(&project);
        lockfile.set_workspace(workspace_packages);
    }

    // Parse freeze file and add packages
    // Filter out workspace packages from external dependencies
    let workspace_names: Vec<_> = project
        .package_names()
        .iter()
        .map(|s| s.to_string())
        .collect();
    let packages = parse_freeze_file(&freeze_content);
    for pkg in packages {
        // Skip workspace packages - they're tracked separately
        if !workspace_names.contains(&pkg.name) {
            lockfile.add_package(pkg);
        }
    }

    // Calculate fingerprint
    let fingerprint = lockfile.fingerprint();
    lockfile.plan.hash = Some(fingerprint);

    // Detect platform
    lockfile.plan.platform = Some(detect_platform());

    // Write lockfile
    let lockfile_path = project.lockfile_path();
    lockfile.to_file(&lockfile_path)?;

    if project.is_workspace() {
        spinner.finish_success(format!(
            "Created {} with {} workspace packages and {} external dependencies",
            LOCKFILE_FILENAME,
            lockfile.workspace.packages.len(),
            lockfile.packages.len()
        ));
    } else {
        spinner.finish_success(format!(
            "Created {} with {} packages",
            LOCKFILE_FILENAME,
            lockfile.packages.len()
        ));
    }

    output.info(&format!("Lockfile: {}", lockfile_path.display()));

    Ok(0)
}

/// Collect workspace package information from the project.
fn collect_workspace_packages(project: &Project) -> Vec<WorkspacePackageInfo> {
    let mut packages = Vec::new();

    for pkg in &project.workspace_packages {
        // Parse version from .cabal file
        let version = parse_cabal_version(&pkg.cabal_file).unwrap_or_else(|| "0.0.0".to_string());

        packages.push(WorkspacePackageInfo {
            name: pkg.name.clone(),
            version,
            path: pkg.path.display().to_string(),
        });
    }

    packages
}

/// Parse the version field from a .cabal file.
fn parse_cabal_version(cabal_path: &Path) -> Option<String> {
    let content = std::fs::read_to_string(cabal_path).ok()?;

    for line in content.lines() {
        let line = line.trim();
        if line.to_lowercase().starts_with("version:") {
            let version = line
                .strip_prefix("version:")
                .or_else(|| line.strip_prefix("Version:"))
                .map(|s| s.trim().to_string());
            return version;
        }
    }

    None
}

/// Run the sync command.
pub async fn sync(force: bool, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    if project.is_workspace() {
        output.status(
            "Syncing",
            &format!("{} ({} packages)", project.name(), project.package_count()),
        );
    } else {
        output.status("Syncing", project.name());
    }

    // Check if lockfile exists
    let lockfile_path = project.lockfile_path();
    if !lockfile_path.exists() {
        output.error(&format!("{} not found", LOCKFILE_FILENAME));
        output.info("Run `hx lock` to create a lockfile first");
        return Ok(3); // Config error
    }

    // Load lockfile
    let lockfile = Lockfile::from_file(&lockfile_path)?;

    // Verify toolchain if not forcing
    if !force {
        let toolchain = Toolchain::detect().await;

        if let (Some(lock_ghc), Some(detected_ghc)) =
            (&lockfile.toolchain.ghc, toolchain.ghc.status.version())
            && lock_ghc != &detected_ghc.to_string()
        {
            output.warn(&format!(
                "GHC version mismatch: lock has {}, detected {}",
                lock_ghc, detected_ghc
            ));
            output.info("Run `hx toolchain install` to install the correct version");
            output.info("Or run `hx sync --force` to override");
        }

        // Verify workspace consistency
        if project.is_workspace() != lockfile.is_workspace() {
            output.warn("Workspace configuration changed since lock was created");
            output.info("Run `hx lock` to regenerate the lockfile");
            if !force {
                return Ok(3);
            }
        }

        // Check if workspace packages have changed
        if lockfile.is_workspace() {
            let current_packages: Vec<_> = project
                .package_names()
                .iter()
                .map(|s| s.to_string())
                .collect();
            let locked_packages = lockfile.workspace_package_names();

            let mut packages_changed = current_packages.len() != locked_packages.len();
            if !packages_changed {
                for pkg in &current_packages {
                    if !locked_packages.contains(&pkg.as_str()) {
                        packages_changed = true;
                        break;
                    }
                }
            }

            if packages_changed {
                output.warn("Workspace packages changed since lock was created");
                output.info("Run `hx lock` to regenerate the lockfile");
                if !force {
                    return Ok(3);
                }
            }
        }
    }

    // Build with locked dependencies
    // For now, just run a regular build since we have the freeze file
    let build_dir = project.cabal_build_dir();

    let options = hx_cabal::BuildOptions {
        release: false,
        jobs: None,
        target: None,
        package: None,
        verbose: output.is_verbose(),
        fingerprint: None,
        ghc_version: None,
        package_count: None,
        project_name: None,
    };

    match hx_cabal::build::build(&project.root, &build_dir, &options, output).await {
        Ok(_) => {
            if project.is_workspace() {
                output.success_summary(
                    &format!(
                        "Synced {} packages with locked dependencies",
                        project.package_count()
                    ),
                    std::time::Duration::from_secs(0),
                );
            } else {
                output.success_summary(
                    "Synced with locked dependencies",
                    std::time::Duration::from_secs(0),
                );
            }
            Ok(0)
        }
        Err(e) => {
            output.print_error(&e);
            Ok(5)
        }
    }
}

fn detect_platform() -> String {
    let arch = std::env::consts::ARCH;
    let os = std::env::consts::OS;
    format!("{}-{}", arch, os)
}
