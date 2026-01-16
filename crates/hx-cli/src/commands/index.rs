//! Index management commands.

use anyhow::Result;
use hx_solver::{MirrorOptions, clear_index, index_is_current, index_status, update_index};
use hx_ui::{Output, Verbosity};
use std::io::{self, Write};

/// Run `hx index update`.
pub async fn update(force: bool, staleness: Option<u64>, output: &Output) -> Result<i32> {
    let staleness_hours = staleness.unwrap_or(24);

    // Check if update is needed
    if !force && index_is_current(staleness_hours) {
        output.info("Index is up to date");
        return Ok(0);
    }

    output.status("Updating", "Hackage index...");

    let options = MirrorOptions {
        force,
        staleness_hours,
        show_progress: output.verbosity() >= Verbosity::Normal,
        ..Default::default()
    };

    match update_index(&options).await {
        Ok(result) => {
            if result.downloaded {
                output.status(
                    "Downloaded",
                    &format!(
                        "index ({:.2} MB)",
                        result.bytes_downloaded as f64 / 1_000_000.0
                    ),
                );
            } else {
                output.info("Index is already up to date");
            }
            Ok(0)
        }
        Err(e) => {
            output.error(&format!("Failed to update index: {}", e));
            Ok(1)
        }
    }
}

/// Run `hx index status`.
pub async fn status(output: &Output) -> Result<i32> {
    match index_status() {
        Ok(status) => {
            output.header("Hackage Index Status");
            println!();
            println!("  Path:         {}", status.path.display());
            println!("  Size:         {}", status.size_string());
            println!("  Last updated: {}", status.state.age_string());
            println!("  SHA256:       {}...", &status.state.sha256[..16]);

            if let Some(count) = status.state.package_count {
                println!("  Packages:     {}", count);
            }
            if let Some(count) = status.state.version_count {
                println!("  Versions:     {}", count);
            }

            // Check staleness
            if status.state.is_stale(24) {
                println!();
                output.warn("Index is stale (>24 hours old). Run `hx index update` to refresh.");
            }

            Ok(0)
        }
        Err(hx_solver::MirrorError::IndexNotFound) => {
            output.warn("No local index found");
            println!();
            println!("  Run `hx index update` to download the Hackage index.");
            println!("  This enables offline dependency resolution.");
            Ok(0)
        }
        Err(e) => {
            output.error(&format!("Failed to get index status: {}", e));
            Ok(1)
        }
    }
}

/// Run `hx index clear`.
pub async fn clear(yes: bool, output: &Output) -> Result<i32> {
    if !yes {
        print!("Clear the local Hackage index? [y/N] ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if !input.trim().eq_ignore_ascii_case("y") {
            output.info("Cancelled");
            return Ok(0);
        }
    }

    match clear_index() {
        Ok(()) => {
            output.status("Cleared", "index");
            Ok(0)
        }
        Err(e) => {
            output.error(&format!("Failed to clear index: {}", e));
            Ok(1)
        }
    }
}
