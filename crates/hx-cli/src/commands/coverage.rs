//! Test coverage command using hpc.
//!
//! Runs tests with coverage instrumentation and generates reports.

use anyhow::{Context, Result, bail};
use serde::Serialize;
use std::path::{Path, PathBuf};
use std::process::Command;

use hx_config::{Project, find_project_root};
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::{Output, Spinner};

/// Coverage configuration.
pub struct CoverageConfig {
    pub html: bool,
    pub open: bool,
    pub output_dir: PathBuf,
    pub threshold: Option<u8>,
    pub package: Option<String>,
    pub pattern: Option<String>,
    pub json: bool,
    pub exclude: Option<String>,
}

/// Coverage summary data.
#[derive(Debug, Default, Serialize)]
pub struct CoverageSummary {
    pub expressions_used: usize,
    pub expressions_total: usize,
    pub expressions_percent: f64,
    pub booleans_used: usize,
    pub booleans_total: usize,
    pub booleans_percent: f64,
    pub alternatives_used: usize,
    pub alternatives_total: usize,
    pub alternatives_percent: f64,
    pub local_declarations_used: usize,
    pub local_declarations_total: usize,
    pub local_declarations_percent: f64,
    pub top_level_declarations_used: usize,
    pub top_level_declarations_total: usize,
    pub top_level_declarations_percent: f64,
}

/// Run the coverage command.
pub async fn run(
    config: CoverageConfig,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    // 1. Find project and load config
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Coverage", project.name());

    // 2. Ensure toolchain
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

    // 3. Build with coverage
    let build_dir = project.cabal_build_dir();
    let spinner = Spinner::new("Building with coverage instrumentation...");

    let mut build_args = vec![
        "build".to_string(),
        "--enable-coverage".to_string(),
        format!("--builddir={}", build_dir.display()),
    ];

    if let Some(ref pkg) = config.package {
        build_args.push(pkg.clone());
    }

    let build_status = Command::new("cabal")
        .args(&build_args)
        .current_dir(&project_root)
        .status()
        .context("Failed to run cabal build")?;

    if !build_status.success() {
        spinner.finish_error("Coverage build failed");
        return Ok(5);
    }
    spinner.finish_success("Coverage build complete");

    // 4. Run tests with coverage
    let spinner = Spinner::new("Running tests with coverage...");

    let mut test_args = vec![
        "test".to_string(),
        "--enable-coverage".to_string(),
        format!("--builddir={}", build_dir.display()),
    ];

    if let Some(ref pkg) = config.package {
        test_args.push(pkg.clone());
    }

    if let Some(ref pattern) = config.pattern {
        test_args.push(format!("--test-option=--match={}", pattern));
    }

    let test_status = Command::new("cabal")
        .args(&test_args)
        .current_dir(&project_root)
        .status()
        .context("Failed to run cabal test")?;

    if !test_status.success() {
        spinner.finish_error("Tests failed");
        return Ok(5);
    }
    spinner.finish_success("Tests passed");

    // 5. Find .tix and .mix files
    let (tix_files, mix_dirs) = find_coverage_files(&build_dir)?;

    if tix_files.is_empty() {
        output.error("No coverage data found (.tix files)");
        output.info("Make sure your test suite is configured correctly");
        return Ok(1);
    }

    output.verbose(&format!("Found {} .tix file(s)", tix_files.len()));

    // 6. Create output directory
    let output_dir = project_root.join(&config.output_dir);
    std::fs::create_dir_all(&output_dir).context("Failed to create coverage output directory")?;

    // 7. Combine tix files if multiple
    let combined_tix = if tix_files.len() > 1 {
        combine_tix_files(&tix_files, &output_dir, output)?
    } else {
        tix_files[0].clone()
    };

    // 8. Generate text report and parse summary
    let summary = generate_text_report(&combined_tix, &mix_dirs, &config, output)?;

    // 9. Generate HTML report if requested
    if config.html {
        generate_html_report(&combined_tix, &mix_dirs, &output_dir, &config, output)?;

        if config.open {
            let index_path = output_dir.join("hpc_index.html");
            if index_path.exists() {
                open_in_browser(&index_path)?;
            }
        }
    }

    // 10. Output JSON if requested
    if config.json {
        let json = serde_json::to_string_pretty(&summary)?;
        let json_path = output_dir.join("coverage.json");
        std::fs::write(&json_path, &json)?;
        output.status("JSON", &json_path.display().to_string());
    }

    // 11. Display summary
    display_summary(&summary, output);

    // 12. Check threshold
    if let Some(threshold) = config.threshold {
        let coverage = summary.expressions_percent;
        if coverage < threshold as f64 {
            output.error(&format!(
                "Coverage {:.1}% is below threshold {}%",
                coverage, threshold
            ));
            return Ok(1);
        }
        output.status(
            "Threshold",
            &format!("{:.1}% >= {}% ✓", coverage, threshold),
        );
    }

    Ok(0)
}

/// Find .tix and .mix files in build directory.
fn find_coverage_files(build_dir: &Path) -> Result<(Vec<PathBuf>, Vec<PathBuf>)> {
    let mut tix_files = Vec::new();
    let mut mix_dirs = Vec::new();

    // Search for .tix files and mix directories
    for entry in walkdir::WalkDir::new(build_dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();

        if path.extension().map(|e| e == "tix").unwrap_or(false) {
            tix_files.push(path.to_path_buf());
        }

        // Look for mix directories
        if path.is_dir() && path.file_name().map(|n| n == "mix").unwrap_or(false) {
            mix_dirs.push(path.to_path_buf());
        }
    }

    // Also check standard hpc location
    let standard_mix = build_dir.join("hpc").join("vanilla").join("mix");
    if standard_mix.exists() && !mix_dirs.contains(&standard_mix) {
        mix_dirs.push(standard_mix);
    }

    Ok((tix_files, mix_dirs))
}

/// Combine multiple .tix files using hpc sum.
fn combine_tix_files(tix_files: &[PathBuf], output_dir: &Path, output: &Output) -> Result<PathBuf> {
    output.verbose("Combining multiple .tix files...");

    let combined_path = output_dir.join("combined.tix");

    let mut args = vec![
        "sum".to_string(),
        "--output".to_string(),
        combined_path.display().to_string(),
    ];
    for tix in tix_files {
        args.push(tix.display().to_string());
    }

    let status = Command::new("hpc")
        .args(&args)
        .status()
        .context("Failed to run hpc sum")?;

    if !status.success() {
        bail!("Failed to combine .tix files");
    }

    Ok(combined_path)
}

/// Generate text report and parse coverage summary.
fn generate_text_report(
    tix_path: &Path,
    mix_dirs: &[PathBuf],
    config: &CoverageConfig,
    _output: &Output,
) -> Result<CoverageSummary> {
    let mut args = vec!["report".to_string()];

    // Add all mix directories
    for mix_dir in mix_dirs {
        args.push(format!("--hpcdir={}", mix_dir.display()));
    }

    args.push(tix_path.display().to_string());

    if let Some(ref exclude) = config.exclude {
        for module in exclude.split(',') {
            args.push(format!("--exclude={}", module.trim()));
        }
    }

    let report_output = Command::new("hpc")
        .args(&args)
        .output()
        .context("Failed to run hpc report")?;

    let report_text = String::from_utf8_lossy(&report_output.stdout);

    // Parse the hpc report output
    parse_coverage_report(&report_text)
}

/// Parse hpc report output into CoverageSummary.
fn parse_coverage_report(report: &str) -> Result<CoverageSummary> {
    let mut summary = CoverageSummary::default();

    // hpc report format:
    //  80% expressions used (80/100)
    //  50% boolean coverage (5/10)
    //  75% alternatives used (15/20)
    //  90% local declarations used (9/10)
    //  100% top-level declarations used (5/5)

    for line in report.lines() {
        let line = line.trim();

        if line.contains("expressions used") {
            if let Some((pct, used, total)) = parse_coverage_line(line) {
                summary.expressions_percent = pct;
                summary.expressions_used = used;
                summary.expressions_total = total;
            }
        } else if line.contains("boolean coverage") {
            if let Some((pct, used, total)) = parse_coverage_line(line) {
                summary.booleans_percent = pct;
                summary.booleans_used = used;
                summary.booleans_total = total;
            }
        } else if line.contains("alternatives used") {
            if let Some((pct, used, total)) = parse_coverage_line(line) {
                summary.alternatives_percent = pct;
                summary.alternatives_used = used;
                summary.alternatives_total = total;
            }
        } else if line.contains("local declarations used") {
            if let Some((pct, used, total)) = parse_coverage_line(line) {
                summary.local_declarations_percent = pct;
                summary.local_declarations_used = used;
                summary.local_declarations_total = total;
            }
        } else if line.contains("top-level declarations used") {
            if let Some((pct, used, total)) = parse_coverage_line(line) {
                summary.top_level_declarations_percent = pct;
                summary.top_level_declarations_used = used;
                summary.top_level_declarations_total = total;
            }
        }
    }

    Ok(summary)
}

/// Parse a single coverage line like "80% expressions used (80/100)"
fn parse_coverage_line(line: &str) -> Option<(f64, usize, usize)> {
    // Extract percentage
    let pct_str = line.split('%').next()?.trim();
    let pct: f64 = pct_str.parse().ok()?;

    // Extract (used/total)
    let parens_start = line.find('(')?;
    let parens_end = line.find(')')?;
    let ratio = &line[parens_start + 1..parens_end];

    let mut parts = ratio.split('/');
    let used: usize = parts.next()?.trim().parse().ok()?;
    let total: usize = parts.next()?.trim().parse().ok()?;

    Some((pct, used, total))
}

/// Generate HTML report using hpc markup.
fn generate_html_report(
    tix_path: &Path,
    mix_dirs: &[PathBuf],
    output_dir: &Path,
    config: &CoverageConfig,
    output: &Output,
) -> Result<()> {
    output.status("Generating", "HTML coverage report");

    let mut args = vec!["markup".to_string()];

    // Add all mix directories
    for mix_dir in mix_dirs {
        args.push(format!("--hpcdir={}", mix_dir.display()));
    }

    args.push(format!("--destdir={}", output_dir.display()));
    args.push(tix_path.display().to_string());

    if let Some(ref exclude) = config.exclude {
        for module in exclude.split(',') {
            args.push(format!("--exclude={}", module.trim()));
        }
    }

    let status = Command::new("hpc")
        .args(&args)
        .status()
        .context("Failed to run hpc markup")?;

    if !status.success() {
        bail!("Failed to generate HTML report");
    }

    let index_path = output_dir.join("hpc_index.html");
    output.status("Report", &index_path.display().to_string());

    Ok(())
}

/// Display coverage summary.
fn display_summary(summary: &CoverageSummary, output: &Output) {
    output.header("Coverage Summary");

    output.list_item(
        "Expressions",
        &format!(
            "{:.1}% ({}/{})",
            summary.expressions_percent, summary.expressions_used, summary.expressions_total
        ),
    );

    output.list_item(
        "Booleans",
        &format!(
            "{:.1}% ({}/{})",
            summary.booleans_percent, summary.booleans_used, summary.booleans_total
        ),
    );

    output.list_item(
        "Alternatives",
        &format!(
            "{:.1}% ({}/{})",
            summary.alternatives_percent, summary.alternatives_used, summary.alternatives_total
        ),
    );

    output.list_item(
        "Local decls",
        &format!(
            "{:.1}% ({}/{})",
            summary.local_declarations_percent,
            summary.local_declarations_used,
            summary.local_declarations_total
        ),
    );

    output.list_item(
        "Top-level decls",
        &format!(
            "{:.1}% ({}/{})",
            summary.top_level_declarations_percent,
            summary.top_level_declarations_used,
            summary.top_level_declarations_total
        ),
    );
}

/// Open a file in the default browser.
fn open_in_browser(path: &Path) -> Result<()> {
    #[cfg(target_os = "macos")]
    {
        Command::new("open")
            .arg(path)
            .status()
            .context("Failed to open browser")?;
    }

    #[cfg(target_os = "linux")]
    {
        Command::new("xdg-open")
            .arg(path)
            .status()
            .context("Failed to open browser")?;
    }

    #[cfg(target_os = "windows")]
    {
        Command::new("cmd")
            .args(["/C", "start", "", &path.display().to_string()])
            .status()
            .context("Failed to open browser")?;
    }

    Ok(())
}
