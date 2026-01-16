//! Publish to Hackage command implementation.

use anyhow::Result;
use hx_config::find_project_root;
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};
use std::fs;

/// Run the publish command.
pub async fn run(
    dry_run: bool,
    username: Option<String>,
    password: Option<String>,
    docs: bool,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;

    // Find the cabal file to get package info
    let cabal_files: Vec<_> = fs::read_dir(&project_root)?
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "cabal")
                .unwrap_or(false)
        })
        .collect();

    if cabal_files.is_empty() {
        output.error("No .cabal file found in project root");
        return Ok(1);
    }

    let cabal_file = &cabal_files[0].path();
    let package_name = cabal_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("package");

    if dry_run {
        output.status("Dry run", "checking package...");
    } else {
        output.status("Publishing", package_name);
    }

    // First, build the sdist
    let spinner = Spinner::new("Creating source distribution...");

    let runner = CommandRunner::new().with_working_dir(&project_root);

    let sdist_output = runner.run("cabal", ["sdist"]).await?;

    if !sdist_output.success() {
        spinner.finish_error("Failed to create source distribution");
        eprintln!("{}", sdist_output.stderr);
        return Ok(1);
    }

    spinner.finish_success("Source distribution created");

    // Check the package
    let spinner = Spinner::new("Checking package...");

    let check_output = runner.run("cabal", ["check"]).await?;

    if !check_output.success() {
        spinner.finish_error("Package check failed");
        eprintln!("{}", check_output.stderr);
        if !dry_run {
            output.warn("Fix the above issues before publishing");
            return Ok(1);
        }
    } else {
        spinner.finish_success("Package check passed");
    }

    if dry_run {
        output.status("Dry run", "complete - package is ready for publishing");
        output.info("Run `hx publish` without --dry-run to upload");
        return Ok(0);
    }

    // Upload to Hackage
    let spinner = Spinner::new("Uploading to Hackage...");

    let mut upload_args = vec!["upload".to_string()];

    // Add credentials if provided
    if let (Some(user), Some(pass)) = (&username, &password) {
        upload_args.push(format!("--username={}", user));
        upload_args.push(format!("--password={}", pass));
    }

    let upload_output = runner
        .run("cabal", upload_args.iter().map(|s| s.as_str()))
        .await?;

    if !upload_output.success() {
        spinner.finish_error("Upload failed");
        eprintln!("{}", upload_output.stderr);

        if upload_output.stderr.contains("401") || upload_output.stderr.contains("authentication") {
            output.info("Hint: Set HACKAGE_USERNAME and HACKAGE_PASSWORD environment variables");
            output.info("Or use --username and --password flags");
        }

        return Ok(1);
    }

    spinner.finish_success("Package uploaded to Hackage");

    // Upload documentation if requested
    if docs {
        let spinner = Spinner::new("Building and uploading documentation...");

        // Build haddock
        let haddock_output = runner
            .run("cabal", ["haddock", "--haddock-for-hackage"])
            .await?;

        if !haddock_output.success() {
            spinner.finish_error("Documentation build failed");
            eprintln!("{}", haddock_output.stderr);
            return Ok(1);
        }

        // Upload docs
        let mut docs_args = vec!["upload".to_string(), "--documentation".to_string()];

        if let (Some(user), Some(pass)) = (&username, &password) {
            docs_args.push(format!("--username={}", user));
            docs_args.push(format!("--password={}", pass));
        }

        let docs_output = runner
            .run("cabal", docs_args.iter().map(|s| s.as_str()))
            .await?;

        if !docs_output.success() {
            spinner.finish_error("Documentation upload failed");
            eprintln!("{}", docs_output.stderr);
            return Ok(1);
        }

        spinner.finish_success("Documentation uploaded");
    }

    output.status("Published", &format!("{} to Hackage", package_name));

    Ok(0)
}
