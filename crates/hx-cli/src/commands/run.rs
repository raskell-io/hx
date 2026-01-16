//! Run command implementation.

use anyhow::Result;
use hx_cabal::build as cabal_build;
use hx_config::{Project, find_project_root};
use hx_ui::Output;

/// Run the project.
pub async fn run(args: Vec<String>, package: Option<String>, output: &Output) -> Result<i32> {
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

    let run_target = match &package {
        Some(pkg) if project.is_workspace() => pkg.as_str(),
        _ => project.name(),
    };

    output.status("Running", run_target);

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::run(&project.root, &build_dir, &args, package.as_deref(), output).await?;
    Ok(exit_code)
}

/// Start a REPL.
pub async fn repl(output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Starting", "REPL");

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::repl(&project.root, &build_dir).await?;
    Ok(exit_code)
}
