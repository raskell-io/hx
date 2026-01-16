//! Self-update functionality.

use anyhow::{Context, Result};
use hx_ui::Output;

const REPO_OWNER: &str = "raskell-io";
const REPO_NAME: &str = "hx";
const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Check for updates or upgrade to latest version.
pub async fn run(check_only: bool, target_version: Option<String>, output: &Output) -> Result<i32> {
    if check_only {
        check_for_updates(output).await
    } else {
        upgrade(target_version, output).await
    }
}

async fn check_for_updates(output: &Output) -> Result<i32> {
    output.status("Checking", "for updates...");

    let latest = get_latest_version().await?;

    if latest == CURRENT_VERSION {
        output.status(
            "Up to date",
            &format!("hx {} is the latest version", CURRENT_VERSION),
        );
        Ok(0)
    } else if is_newer(&latest, CURRENT_VERSION) {
        output.info(&format!(
            "Update available: {} -> {}",
            CURRENT_VERSION, latest
        ));
        output.info("Run `hx upgrade` to install");
        Ok(0)
    } else {
        output.status(
            "Up to date",
            &format!(
                "hx {} is newer than latest release {}",
                CURRENT_VERSION, latest
            ),
        );
        Ok(0)
    }
}

async fn upgrade(target_version: Option<String>, output: &Output) -> Result<i32> {
    let target = match target_version {
        Some(v) => v,
        None => {
            output.status("Checking", "for latest version...");
            get_latest_version().await?
        }
    };

    if target == CURRENT_VERSION {
        output.status(
            "Up to date",
            &format!("hx {} is already installed", CURRENT_VERSION),
        );
        return Ok(0);
    }

    output.status(
        "Upgrading",
        &format!("hx {} -> {}", CURRENT_VERSION, target),
    );

    // Use self_update to download and install
    let status = self_update::backends::github::Update::configure()
        .repo_owner(REPO_OWNER)
        .repo_name(REPO_NAME)
        .bin_name("hx")
        .target(&get_target())
        .current_version(CURRENT_VERSION)
        .target_version_tag(&format!("v{}", target))
        .show_download_progress(true)
        .no_confirm(true)
        .build()
        .context("Failed to configure updater")?
        .update()
        .context("Failed to update")?;

    match status {
        self_update::Status::UpToDate(_) => {
            output.status(
                "Up to date",
                &format!("hx {} is already installed", CURRENT_VERSION),
            );
        }
        self_update::Status::Updated(v) => {
            output.status("Upgraded", &format!("Successfully upgraded to hx {}", v));
        }
    }

    Ok(0)
}

async fn get_latest_version() -> Result<String> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        REPO_OWNER, REPO_NAME
    );

    let client = reqwest::Client::builder()
        .user_agent(format!("hx/{}", CURRENT_VERSION))
        .build()?;

    let response: serde_json::Value = client
        .get(&url)
        .send()
        .await
        .context("Failed to fetch release info")?
        .json()
        .await
        .context("Failed to parse release info")?;

    let tag = response["tag_name"]
        .as_str()
        .context("No tag_name in release")?;

    // Strip leading 'v' if present
    Ok(tag.strip_prefix('v').unwrap_or(tag).to_string())
}

fn is_newer(a: &str, b: &str) -> bool {
    // Simple semver comparison
    let parse = |s: &str| -> (u32, u32, u32) {
        let parts: Vec<u32> = s.split('.').filter_map(|p| p.parse().ok()).collect();
        (
            parts.first().copied().unwrap_or(0),
            parts.get(1).copied().unwrap_or(0),
            parts.get(2).copied().unwrap_or(0),
        )
    };

    parse(a) > parse(b)
}

fn get_target() -> String {
    let arch = std::env::consts::ARCH;
    let os = std::env::consts::OS;

    match (arch, os) {
        ("x86_64", "linux") => "x86_64-unknown-linux-gnu".to_string(),
        ("aarch64", "linux") => "aarch64-unknown-linux-gnu".to_string(),
        ("x86_64", "macos") => "x86_64-apple-darwin".to_string(),
        ("aarch64", "macos") => "aarch64-apple-darwin".to_string(),
        ("x86_64", "windows") => "x86_64-pc-windows-msvc".to_string(),
        _ => format!("{}-unknown-{}", arch, os),
    }
}
