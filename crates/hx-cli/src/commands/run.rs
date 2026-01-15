//! Run command implementation.

use anyhow::Result;
use hx_cabal::build as cabal_build;
use hx_config::{Project, find_project_root};
use hx_ui::Output;

/// Run the project.
pub async fn run(args: Vec<String>, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Running", project.name());

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::run(&project.root, &build_dir, &args, output).await?;
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
