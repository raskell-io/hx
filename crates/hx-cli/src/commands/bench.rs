//! Benchmark command implementation.

use anyhow::Result;
use hx_cache::cabal_store_dir;
use hx_config::{Project, find_project_root};
use hx_core::CommandRunner;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::{Output, Spinner};

/// Run benchmarks.
pub async fn run(
    filter: Option<String>,
    save_baseline: Option<String>,
    baseline: Option<String>,
    package: Option<String>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;
    let build_dir = project_root.join(".hx").join("dist");
    let store_dir = cabal_store_dir()?;

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

    let bench_target = match &package {
        Some(pkg) if project.is_workspace() => pkg.as_str(),
        _ => project.name(),
    };

    output.status("Benchmarking", bench_target);

    // Build benchmark arguments
    let mut args = vec![
        "bench".to_string(),
        format!("--builddir={}", build_dir.display()),
        format!("--store-dir={}", store_dir.display()),
    ];

    // Add package filter for workspace benchmarks
    if let Some(ref pkg) = package
        && project.is_workspace()
    {
        args.push(format!("{}:bench", pkg));
    }

    // Add benchmark-specific options via --benchmark-options
    let mut bench_opts = Vec::new();

    if let Some(ref f) = filter {
        bench_opts.push(format!("--match \"{}\"", f));
    }

    if let Some(ref name) = save_baseline {
        bench_opts.push(format!("--output {}.html", name));
        bench_opts.push(format!("--csv {}.csv", name));
    }

    if let Some(ref name) = baseline {
        output.info(&format!("Comparing against baseline: {}", name));
        // Criterion uses --baseline for comparison
        bench_opts.push(format!("--baseline {}", name));
    }

    if !bench_opts.is_empty() {
        args.push(format!("--benchmark-options=\"{}\"", bench_opts.join(" ")));
    }

    let spinner = Spinner::new("Running benchmarks...");

    let runner = CommandRunner::new().with_working_dir(&project_root);
    let cmd_output = runner.run("cabal", args.iter().map(|s| s.as_str())).await?;

    if cmd_output.success() {
        spinner.finish_success("Benchmarks completed");

        if output.is_verbose() {
            println!("{}", cmd_output.stdout);
        }

        if let Some(ref name) = save_baseline {
            output.info(&format!("Baseline saved: {}.html, {}.csv", name, name));
        }

        Ok(0)
    } else {
        spinner.finish_error("Benchmarks failed");
        eprintln!("{}", cmd_output.stderr);
        Ok(1)
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
