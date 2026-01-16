//! Fetch command implementation.

use anyhow::Result;
use hx_cabal::fetch::{self, FetchOptions};
use hx_config::{Project, find_project_root};
use hx_lock::Lockfile;
use hx_ui::Output;

/// Run the fetch command.
///
/// Pre-downloads all dependencies in parallel, speeding up subsequent builds
/// by separating network I/O from compilation.
pub async fn run(jobs: Option<usize>, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Try to read lockfile for specific package versions
    let lockfile_path = project.lockfile_path();
    let lockfile_packages: Vec<(String, String)> = if lockfile_path.exists() {
        match Lockfile::from_file(&lockfile_path) {
            Ok(lock) => lock
                .packages
                .iter()
                .map(|pkg| (pkg.name.clone(), pkg.version.clone()))
                .collect(),
            Err(e) => {
                output.verbose(&format!("Could not read lockfile: {}", e));
                vec![]
            }
        }
    } else {
        vec![]
    };

    let package_count = if lockfile_packages.is_empty() {
        "all".to_string()
    } else {
        lockfile_packages.len().to_string()
    };

    if project.is_workspace() {
        output.status(
            "Fetching",
            &format!(
                "{} dependencies for {} packages",
                package_count,
                project.package_count()
            ),
        );
    } else {
        output.status("Fetching", &format!("{} dependencies", package_count));
    }

    let build_dir = project.cabal_build_dir();

    let options = FetchOptions {
        jobs,
        verbose: output.is_verbose(),
        packages: vec![], // Will use lockfile if available, otherwise all deps
    };

    // Use lockfile-based fetch if we have a lockfile
    let result = if !lockfile_packages.is_empty() {
        fetch::fetch_from_lockfile(
            &project.root,
            &build_dir,
            &lockfile_packages,
            &options,
            output,
        )
        .await
    } else {
        // Fall back to fetching all dependencies
        fetch::fetch(&project.root, &build_dir, &options, output).await
    };

    match result {
        Ok(result) => {
            if result.packages_fetched > 0 || result.packages_cached > 0 {
                output.info(&format!(
                    "Ready for build: {} packages available",
                    result.packages_fetched + result.packages_cached
                ));
            }
            Ok(0)
        }
        Err(e) => {
            output.print_error(&e);
            Ok(1)
        }
    }
}
