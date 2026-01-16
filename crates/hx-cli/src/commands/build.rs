//! Build command implementation.

use anyhow::Result;
use hx_cabal::build::{self as cabal_build, BuildOptions};
use hx_config::{Project, find_project_root};
use hx_ui::Output;

/// Run the build command.
pub async fn run(
    release: bool,
    jobs: Option<usize>,
    target: Option<String>,
    package: Option<String>,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

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

    let options = BuildOptions {
        release,
        jobs,
        target,
        package: package.clone(),
        verbose: output.is_verbose(),
    };

    let build_dir = project.cabal_build_dir();

    match cabal_build::build(&project.root, &build_dir, &options, output).await {
        Ok(_result) => Ok(0),
        Err(e) => {
            output.print_error(&e);
            Ok(5) // Build error exit code
        }
    }
}

/// Run the test command.
pub async fn test(
    pattern: Option<String>,
    package: Option<String>,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

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
