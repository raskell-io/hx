//! Profiling command implementation.

use anyhow::{Context, Result};
use hx_config::{Project, find_project_root};
use hx_core::CommandRunner;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::{Output, Spinner};
use std::fs;
use std::path::PathBuf;

/// Run the profile command.
pub async fn run(
    heap: bool,
    time: bool,
    detail: u8,
    args: Vec<String>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Ensure toolchain is available
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
        return Ok(4);
    }

    output.status("Profiling", project.name());

    let runner = CommandRunner::new().with_working_dir(&project_root);
    let build_dir = project.cabal_build_dir();

    // Build with profiling enabled
    let spinner = Spinner::new("Building with profiling enabled...");

    let build_result = runner
        .run(
            "cabal",
            [
                "build",
                "--enable-profiling",
                "--ghc-options=-fprof-auto",
                &format!("--builddir={}", build_dir.display()),
            ],
        )
        .await?;

    if !build_result.success() {
        spinner.finish_error("Profiling build failed");
        eprintln!("{}", build_result.stderr);
        return Ok(5);
    }

    spinner.finish_success("Profiling build complete");

    // Find the executable
    let exe_path = find_profiled_executable(&build_dir, project.name())?;

    if !exe_path.exists() {
        output.error("Could not find profiled executable");
        output.info("Make sure your project has an executable component");
        return Ok(1);
    }

    output.status("Running", &format!("{} with profiling", project.name()));

    // Build RTS options
    let mut rts_opts = Vec::new();

    // Time profiling (default if neither heap nor time specified)
    if time || !heap {
        rts_opts.push("-p".to_string());
        rts_opts.push(format!("-P{}", detail.min(3)));
    }

    // Heap profiling
    if heap {
        rts_opts.push("-hc".to_string()); // By cost center
    }

    // Build the full command
    let mut run_args: Vec<String> = vec![exe_path.to_string_lossy().to_string()];
    run_args.extend(args);
    run_args.push("+RTS".to_string());
    run_args.extend(rts_opts);
    run_args.push("-RTS".to_string());

    output.verbose(&format!("Running: {}", run_args.join(" ")));

    // Run the profiled executable
    let status = std::process::Command::new(&run_args[0])
        .args(&run_args[1..])
        .current_dir(&project_root)
        .status()
        .context("Failed to run profiled executable")?;

    if !status.success() {
        output.warn("Program exited with non-zero status");
    }

    // Find and display profiling output
    let prof_file = project_root.join(format!("{}.prof", project.name()));
    let hp_file = project_root.join(format!("{}.hp", project.name()));

    if prof_file.exists() {
        output.status("Generated", &prof_file.display().to_string());
        display_profile_summary(&prof_file, output)?;
    }

    if hp_file.exists() {
        output.status("Generated", &hp_file.display().to_string());
        output.info("Run `hp2ps -c <file>.hp` to generate PostScript graph");
    }

    Ok(0)
}

/// Find the profiled executable in the build directory.
fn find_profiled_executable(build_dir: &std::path::Path, name: &str) -> Result<PathBuf> {
    // Look for the executable in common locations
    let candidates = [
        build_dir.join("build").join(name).join(name),
        build_dir
            .join("x")
            .join(name)
            .join("build")
            .join(name)
            .join(name),
    ];

    for candidate in &candidates {
        if candidate.exists() {
            return Ok(candidate.clone());
        }
    }

    // Try to find it with a glob pattern
    let pattern = build_dir.join("**").join(name);
    if let Ok(paths) = glob::glob(&pattern.to_string_lossy()) {
        for path in paths.flatten() {
            if path.is_file() && is_executable(&path) {
                return Ok(path);
            }
        }
    }

    // Return expected path even if not found
    Ok(candidates[0].clone())
}

/// Check if a file is executable.
fn is_executable(path: &std::path::Path) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if let Ok(metadata) = fs::metadata(path) {
            return metadata.permissions().mode() & 0o111 != 0;
        }
    }

    #[cfg(not(unix))]
    {
        // On Windows, check for .exe extension
        path.extension()
            .map(|e| e == "exe")
            .unwrap_or(false)
    }

    false
}

/// Display a summary of the profiling results.
fn display_profile_summary(prof_file: &std::path::Path, output: &Output) -> Result<()> {
    let content = fs::read_to_string(prof_file)?;

    output.header("Profile Summary");

    // Find the top cost centers
    let mut in_cost_center_section = false;
    let mut top_entries = Vec::new();

    for line in content.lines() {
        if line.contains("COST CENTRE") && line.contains("%time") {
            in_cost_center_section = true;
            continue;
        }

        if in_cost_center_section {
            if line.trim().is_empty() {
                break;
            }

            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 5
                && let (Ok(time), Ok(alloc)) = (
                    parts.get(3).unwrap_or(&"0").parse::<f64>(),
                    parts.get(4).unwrap_or(&"0").parse::<f64>(),
                )
                && (time > 0.1 || alloc > 0.1)
            {
                top_entries.push((
                    parts[0].to_string(),
                    parts[1].to_string(),
                    time,
                    alloc,
                ));
            }
        }
    }

    // Sort by time and show top entries
    top_entries.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap_or(std::cmp::Ordering::Equal));

    if top_entries.is_empty() {
        output.info("No significant cost centers found");
    } else {
        output.info("Top cost centers by time:");
        for (name, module, time, alloc) in top_entries.iter().take(10) {
            output.list_item(
                name,
                &format!("{} - {:.1}% time, {:.1}% alloc", module, time, alloc),
            );
        }
    }

    // Look for total time
    for line in content.lines() {
        if line.contains("total time") {
            output.info(&format!("  {}", line.trim()));
            break;
        }
    }

    for line in content.lines() {
        if line.contains("total alloc") {
            output.info(&format!("  {}", line.trim()));
            break;
        }
    }

    Ok(())
}
