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
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Building", project.name());

    let options = BuildOptions {
        release,
        jobs,
        target,
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
pub async fn test(pattern: Option<String>, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Testing", project.name());

    let build_dir = project.cabal_build_dir();

    match cabal_build::test(&project.root, &build_dir, pattern.as_deref(), output).await {
        Ok(_result) => Ok(0),
        Err(e) => {
            output.print_error(&e);
            Ok(5) // Test error exit code
        }
    }
}
