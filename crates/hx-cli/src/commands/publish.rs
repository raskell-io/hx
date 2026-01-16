//! Publish to Hackage command implementation.

use anyhow::Result;
use hx_config::{Project, find_project_root};
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};
use std::fs;
use std::path::Path;

/// Run the publish command.
pub async fn run(
    dry_run: bool,
    username: Option<String>,
    password: Option<String>,
    docs: bool,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let _project = Project::load(&project_root)?;

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

    output.status("Pre-publish", "running checks...");

    // Run pre-publish checks
    let mut issues = Vec::new();

    // Check 1: Version in cabal file
    let cabal_version = extract_cabal_version(cabal_file);
    if let Some(version) = &cabal_version {
        output.list_item("version", version);
    } else {
        issues.push("Could not extract version from .cabal file");
    }

    // Check 2: Changelog exists and mentions version
    let changelog_path = project_root.join("CHANGELOG.md");
    if changelog_path.exists() {
        if let Some(version) = &cabal_version {
            let changelog = fs::read_to_string(&changelog_path).unwrap_or_default();
            if changelog.contains(version) {
                output.list_item("changelog", &format!("mentions v{}", version));
            } else {
                output.list_item("changelog", "exists (version not mentioned)");
                issues.push("CHANGELOG.md does not mention the current version");
            }
        } else {
            output.list_item("changelog", "exists");
        }
    } else {
        output.list_item("changelog", "missing");
        issues.push("No CHANGELOG.md found (run `hx changelog` to generate)");
    }

    // Check 3: Git status clean
    let runner = CommandRunner::new().with_working_dir(&project_root);
    let git_status = runner.run("git", ["status", "--porcelain"]).await;
    if let Ok(status) = git_status {
        if status.stdout.trim().is_empty() {
            output.list_item("git status", "clean");
        } else {
            output.list_item("git status", "uncommitted changes");
            issues.push("Working directory has uncommitted changes");
        }
    }

    // Check 4: Git tag matches version
    if let Some(version) = &cabal_version {
        let expected_tag = format!("v{}", version);
        let tag_check = runner.run("git", ["tag", "-l", &expected_tag]).await;
        if let Ok(tag_output) = tag_check {
            if tag_output.stdout.trim() == expected_tag {
                output.list_item("git tag", &format!("{} exists", expected_tag));
            } else {
                output.list_item("git tag", &format!("{} missing", expected_tag));
                issues.push("Git tag for current version does not exist");
            }
        }
    }

    // Check 5: Tests pass
    if !dry_run {
        let spinner = Spinner::new("Running tests...");
        let test_output = runner.run("cabal", ["test"]).await;
        match test_output {
            Ok(result) if result.success() => {
                spinner.finish_success("Tests passed");
            }
            Ok(result) => {
                spinner.finish_error("Tests failed");
                if !result.stderr.is_empty() {
                    output.verbose(&result.stderr);
                }
                issues.push("Tests failed");
            }
            Err(_) => {
                spinner.finish_error("Could not run tests");
            }
        }
    }

    // Report issues
    if !issues.is_empty() {
        output.warn(&format!("{} issue(s) found:", issues.len()));
        for issue in &issues {
            output.info(&format!("  - {}", issue));
        }

        if !dry_run {
            output.error("Fix issues before publishing, or use --dry-run to see all checks");
            return Ok(1);
        }
    }

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

/// Extract version from a .cabal file.
fn extract_cabal_version(cabal_file: &Path) -> Option<String> {
    let content = fs::read_to_string(cabal_file).ok()?;

    for line in content.lines() {
        let trimmed = line.trim().to_lowercase();
        if trimmed.starts_with("version:") {
            let version = line
                .trim()
                .strip_prefix("version:")
                .or_else(|| line.trim().strip_prefix("Version:"))
                .map(|v| v.trim().to_string());
            return version;
        }
    }

    None
}
