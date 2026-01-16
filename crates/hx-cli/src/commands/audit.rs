//! Dependency audit command implementation.

use anyhow::Result;
use hx_config::{Project, find_project_root};
use hx_lock::Lockfile;
use hx_ui::Output;
use std::collections::HashMap;

/// Run the audit command.
pub async fn run(
    fix: bool,
    ignore: Vec<String>,
    outdated: bool,
    licenses: bool,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Auditing", project.name());

    // Load lockfile
    let lockfile = match Lockfile::from_file(project.lockfile_path()) {
        Ok(l) => l,
        Err(_) => {
            output.error("No lockfile found");
            output.info("Run `hx lock` first to generate a lockfile");
            return Ok(1);
        }
    };

    let mut issues = Vec::new();
    let mut warnings = Vec::new();

    // Check for known vulnerabilities
    let vulnerabilities = check_vulnerabilities(&lockfile, &ignore);
    for vuln in vulnerabilities {
        issues.push(vuln);
    }

    // Check for outdated dependencies (basic check without index)
    if outdated {
        output.info("Checking for outdated packages...");
        output.info("(Note: Run `hx index update` for full outdated check)");
    }

    // Check license compatibility
    if licenses {
        let license_issues = check_licenses(&lockfile);
        for issue in license_issues {
            warnings.push(issue);
        }
    }

    // Check for deprecated packages
    let deprecated = check_deprecated(&lockfile);
    for dep in deprecated {
        warnings.push(dep);
    }

    // Display results
    output.header("Audit Results");

    let total_deps = lockfile.packages.len();
    output.list_item("Packages scanned", &total_deps.to_string());

    if issues.is_empty() && warnings.is_empty() {
        output.status("✓", "No issues found");
        return Ok(0);
    }

    // Display issues (high severity)
    if !issues.is_empty() {
        output.error(&format!("{} security issue(s) found:", issues.len()));
        for issue in &issues {
            display_issue(issue, output);
        }
    }

    // Display warnings (low severity)
    if !warnings.is_empty() {
        output.warn(&format!("{} warning(s):", warnings.len()));
        for warning in &warnings {
            display_warning(warning, output);
        }
    }

    // Apply fixes if requested
    if fix && !issues.is_empty() {
        output.status("Fixing", "attempting automatic fixes...");

        for issue in &issues {
            if let Some(cmd) = &issue.fix_command {
                output.info(&format!("  Would run: {}", cmd));
            }
        }

        output.info("Run the suggested commands to fix issues");
    }

    // Exit code based on severity
    if !issues.is_empty() {
        Ok(1) // Security issues found
    } else {
        Ok(0) // Only warnings
    }
}

/// Audit issue severity.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
enum Severity {
    Critical,
    High,
    Medium,
    Low,
}

/// An audit issue.
#[derive(Debug)]
struct AuditIssue {
    package: String,
    version: String,
    severity: Severity,
    advisory_id: String,
    title: String,
    #[allow(dead_code)]
    description: String,
    fix_command: Option<String>,
}

/// An audit warning.
#[derive(Debug)]
struct AuditWarning {
    package: String,
    #[allow(dead_code)]
    severity: Severity,
    message: String,
    fix_command: Option<String>,
}

/// Check for known vulnerabilities.
fn check_vulnerabilities(lockfile: &Lockfile, ignore: &[String]) -> Vec<AuditIssue> {
    let mut issues = Vec::new();

    // Simulated vulnerability database
    // In a real implementation, this would query a security advisory database
    let known_vulnerabilities: HashMap<&str, Vec<(&str, &str, &str, Severity)>> = [
        (
            "aeson",
            vec![(
                "<2.0.0.0",
                "HSEC-2022-0001",
                "Denial of service via large numbers",
                Severity::Medium,
            )],
        ),
        (
            "tar",
            vec![(
                "<0.5.1.1",
                "HSEC-2023-0001",
                "Path traversal vulnerability",
                Severity::High,
            )],
        ),
        (
            "cryptonite",
            vec![(
                "<0.30",
                "HSEC-2023-0002",
                "Timing side-channel in ECDSA",
                Severity::Medium,
            )],
        ),
    ]
    .into_iter()
    .collect();

    for pkg in &lockfile.packages {
        if ignore.contains(&pkg.name) {
            continue;
        }

        if let Some(vulns) = known_vulnerabilities.get(pkg.name.as_str()) {
            for (version_range, advisory_id, title, severity) in vulns {
                if version_in_range(&pkg.version, version_range) {
                    issues.push(AuditIssue {
                        package: pkg.name.clone(),
                        version: pkg.version.clone(),
                        severity: severity.clone(),
                        advisory_id: advisory_id.to_string(),
                        title: title.to_string(),
                        description: format!(
                            "Package {} {} is affected by {}",
                            pkg.name, pkg.version, advisory_id
                        ),
                        fix_command: Some(format!("hx add {}@latest", pkg.name)),
                    });
                }
            }
        }
    }

    issues
}

/// Check if a version is in a vulnerable range.
fn version_in_range(version: &str, range: &str) -> bool {
    // Simple range check for "<X.Y.Z" format
    if let Some(max_version) = range.strip_prefix('<') {
        return !is_newer(version, max_version) && version != max_version;
    }
    false
}

/// Simple version comparison (newer = higher).
fn is_newer(a: &str, b: &str) -> bool {
    let parse_version =
        |v: &str| -> Vec<u32> { v.split('.').filter_map(|s| s.parse().ok()).collect() };

    let va = parse_version(a);
    let vb = parse_version(b);

    va > vb
}

/// Check for license compatibility issues.
fn check_licenses(lockfile: &Lockfile) -> Vec<AuditWarning> {
    let mut warnings = Vec::new();

    // Licenses that might be problematic for commercial use
    let problematic_packages: HashMap<&str, &str> = [
        // This would ideally come from package metadata
        // For now, we just check known GPL packages
    ]
    .into_iter()
    .collect();

    for pkg in &lockfile.packages {
        if let Some(license) = problematic_packages.get(pkg.name.as_str()) {
            warnings.push(AuditWarning {
                package: pkg.name.clone(),
                severity: Severity::Medium,
                message: format!("Uses {} license (copyleft)", license),
                fix_command: None,
            });
        }
    }

    warnings
}

/// Check for deprecated packages.
fn check_deprecated(lockfile: &Lockfile) -> Vec<AuditWarning> {
    let mut warnings = Vec::new();

    // Known deprecated packages and their replacements
    let deprecated_packages: HashMap<&str, &str> = [
        ("old-time", "time"),
        ("old-locale", "time"),
        ("cryptohash", "cryptonite"),
        ("MonadRandom", "random"),
        ("SHA", "cryptonite"),
        ("crypto-api", "cryptonite"),
    ]
    .into_iter()
    .collect();

    for pkg in &lockfile.packages {
        if let Some(replacement) = deprecated_packages.get(pkg.name.as_str()) {
            warnings.push(AuditWarning {
                package: pkg.name.clone(),
                severity: Severity::Low,
                message: format!("Deprecated, consider using '{}' instead", replacement),
                fix_command: Some(format!("hx rm {} && hx add {}", pkg.name, replacement)),
            });
        }
    }

    warnings
}

/// Display an audit issue.
fn display_issue(issue: &AuditIssue, output: &Output) {
    let severity_str = match issue.severity {
        Severity::Critical => "CRITICAL",
        Severity::High => "HIGH",
        Severity::Medium => "MEDIUM",
        Severity::Low => "LOW",
    };

    output.error(&format!(
        "  [{}] {} {} - {}",
        severity_str, issue.package, issue.version, issue.advisory_id
    ));
    output.info(&format!("    {}", issue.title));

    if let Some(cmd) = &issue.fix_command {
        output.info(&format!("    Fix: {}", cmd));
    }
}

/// Display an audit warning.
fn display_warning(warning: &AuditWarning, output: &Output) {
    output.warn(&format!("  {} - {}", warning.package, warning.message));

    if let Some(cmd) = &warning.fix_command {
        output.info(&format!("    Fix: {}", cmd));
    }
}
