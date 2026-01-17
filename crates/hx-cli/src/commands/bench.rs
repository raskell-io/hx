//! Benchmark command implementation.

use anyhow::Result;
use hx_cache::cabal_store_dir;
use hx_config::{Project, find_project_root};
use hx_core::CommandRunner;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::{Output, Spinner};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Benchmark result for a single benchmark.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub name: String,
    pub mean_ns: f64,
    pub std_dev_ns: f64,
    pub throughput: Option<f64>,
}

/// Benchmark run results.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkRun {
    pub timestamp: String,
    pub git_commit: Option<String>,
    pub git_branch: Option<String>,
    pub package: String,
    pub results: Vec<BenchmarkResult>,
}

/// Historical benchmark entry.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BenchmarkHistory {
    pub runs: Vec<BenchmarkRun>,
}

/// Run benchmarks.
#[allow(clippy::too_many_arguments)]
pub async fn run(
    filter: Option<String>,
    save_baseline: Option<String>,
    baseline: Option<String>,
    package: Option<String>,
    json: bool,
    regression_threshold: Option<f64>,
    save_history: bool,
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

    if !json {
        output.status("Benchmarking", bench_target);
    }

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

    // For JSON output, ask Criterion to output JSON
    if json {
        bench_opts.push("--format json".to_string());
    }

    if let Some(ref name) = save_baseline {
        bench_opts.push(format!("--output {}.html", name));
        bench_opts.push(format!("--csv {}.csv", name));
    }

    if let Some(ref name) = baseline {
        if !json {
            output.info(&format!("Comparing against baseline: {}", name));
        }
        // Criterion uses --baseline for comparison
        bench_opts.push(format!("--baseline {}", name));
    }

    if !bench_opts.is_empty() {
        args.push(format!("--benchmark-options=\"{}\"", bench_opts.join(" ")));
    }

    let spinner = if !json {
        Some(Spinner::new("Running benchmarks..."))
    } else {
        None
    };

    let runner = CommandRunner::new().with_working_dir(&project_root);
    let cmd_output = runner.run("cabal", args.iter().map(|s| s.as_str())).await?;

    if cmd_output.success() {
        if let Some(s) = spinner {
            s.finish_success("Benchmarks completed");
        }

        // Parse and display results
        if json {
            // Output raw benchmark output as JSON
            println!("{}", cmd_output.stdout);
        } else if output.is_verbose() {
            println!("{}", cmd_output.stdout);
        }

        // Save to history if requested
        if save_history {
            save_benchmark_history(&project_root, bench_target, &cmd_output.stdout)?;
            if !json {
                output.info("Benchmark results saved to history");
            }
        }

        if let Some(ref name) = save_baseline {
            if !json {
                output.info(&format!("Baseline saved: {}.html, {}.csv", name, name));
            }
        }

        // Check for regressions if threshold is set and we have a baseline
        if let (Some(threshold), Some(_baseline_name)) = (regression_threshold, &baseline) {
            // Parse benchmark output to check for regressions
            if let Some(regression) = check_for_regression(&cmd_output.stdout, threshold) {
                if !json {
                    output.error(&format!(
                        "Performance regression detected: {:.1}% (threshold: {:.1}%)",
                        regression, threshold
                    ));
                }
                return Ok(2); // Regression detected exit code
            }
        }

        Ok(0)
    } else {
        if let Some(s) = spinner {
            s.finish_error("Benchmarks failed");
        }
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

/// Save benchmark results to history.
fn save_benchmark_history(project_root: &Path, package: &str, _output: &str) -> Result<()> {
    let history_dir = project_root.join(".hx").join("benchmarks");
    fs::create_dir_all(&history_dir)?;

    let history_file = history_dir.join("history.json");

    // Load existing history or create new
    let mut history: BenchmarkHistory = if history_file.exists() {
        let content = fs::read_to_string(&history_file)?;
        serde_json::from_str(&content).unwrap_or_default()
    } else {
        BenchmarkHistory::default()
    };

    // Get git info if available
    let git_commit = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string());

    let git_branch = std::process::Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string());

    // Create new run entry
    let run = BenchmarkRun {
        timestamp: chrono::Utc::now().to_rfc3339(),
        git_commit,
        git_branch,
        package: package.to_string(),
        results: Vec::new(), // Would need to parse criterion output
    };

    history.runs.push(run);

    // Keep only last 100 runs
    if history.runs.len() > 100 {
        history.runs = history.runs.split_off(history.runs.len() - 100);
    }

    // Save history
    let content = serde_json::to_string_pretty(&history)?;
    fs::write(&history_file, content)?;

    Ok(())
}

/// Check for performance regressions in benchmark output.
fn check_for_regression(output: &str, threshold: f64) -> Option<f64> {
    // Look for Criterion's regression detection in output
    // Criterion outputs lines like: "Performance has regressed by 15.2%"
    for line in output.lines() {
        if line.contains("regressed") {
            // Try to extract percentage
            if let Some(pct_str) = line
                .split_whitespace()
                .find(|s| s.ends_with('%'))
                .map(|s| s.trim_end_matches('%'))
            {
                if let Ok(pct) = pct_str.parse::<f64>() {
                    if pct > threshold {
                        return Some(pct);
                    }
                }
            }
        }
    }
    None
}
