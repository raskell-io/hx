//! BHC Platform command implementation.

use anyhow::Result;
use hx_config::{MANIFEST_FILENAME, Manifest, find_project_root};
use hx_solver::bhc_platform;
use hx_solver::snapshot::SnapshotId;
use hx_ui::Output;

/// List available BHC Platform snapshots.
pub async fn list(output: &Output) -> Result<i32> {
    let platforms = bhc_platform::list_platforms();

    if platforms.is_empty() {
        output.info("No BHC Platform snapshots available");
        return Ok(0);
    }

    output.status("Available", "BHC Platform snapshots");
    output.info("");

    for platform in &platforms {
        output.info(&format!(
            "  {} (BHC {}, GHC-compat {}, {} packages)",
            platform.id, platform.bhc_version, platform.ghc_compat, platform.package_count,
        ));
    }

    output.info("");
    output.info("Set a platform with: hx bhc-platform set <platform>");

    Ok(0)
}

/// Show information about a BHC Platform snapshot.
pub async fn info(platform_str: &str, show_packages: bool, output: &Output) -> Result<i32> {
    let snapshot_id = match SnapshotId::parse(platform_str) {
        Ok(id) => id,
        Err(e) => {
            output.error(&format!(
                "Invalid platform identifier '{}': {}",
                platform_str, e
            ));
            output.info("Example: bhc-platform-2026.1");
            return Ok(1);
        }
    };

    let snapshot = match bhc_platform::load_bhc_platform(&snapshot_id) {
        Ok(snap) => snap,
        Err(e) => {
            output.error(&format!("Failed to load platform: {}", e));
            return Ok(1);
        }
    };

    output.status("Platform", &snapshot_id.key());
    output.info("");
    output.info(&format!(
        "  GHC compatibility: {}",
        snapshot.metadata.ghc_version
    ));
    output.info(&format!(
        "  Packages:          {}",
        snapshot.metadata.package_count
    ));

    if let Some(ref bhc_ver) = snapshot.metadata.bhc_version {
        output.info(&format!("  BHC version:       {}", bhc_ver));
    }
    if let Some(ref profile) = snapshot.metadata.recommended_profile {
        output.info(&format!("  Recommended profile: {}", profile));
    }
    if let Some(ref created) = snapshot.metadata.created {
        output.info(&format!("  Created:           {}", created));
    }

    if show_packages {
        output.info("");
        output.info("Packages:");

        let mut packages: Vec<_> = snapshot.packages.values().collect();
        packages.sort_by(|a, b| a.name.cmp(&b.name));

        for pkg in &packages {
            output.info(&format!("  {} {}", pkg.name, pkg.version));
        }
    }

    Ok(0)
}

/// Set the BHC Platform snapshot for the current project.
pub async fn set(platform_str: &str, output: &Output) -> Result<i32> {
    // Validate the platform first
    let snapshot_id = match SnapshotId::parse(platform_str) {
        Ok(id) => id,
        Err(e) => {
            output.error(&format!(
                "Invalid platform identifier '{}': {}",
                platform_str, e
            ));
            output.info("Example: bhc-platform-2026.1");
            return Ok(1);
        }
    };

    // Try to load the platform to validate it exists
    let snapshot = match bhc_platform::load_bhc_platform(&snapshot_id) {
        Ok(snap) => snap,
        Err(e) => {
            output.error(&format!("Platform not found: {}", e));
            return Ok(1);
        }
    };

    // Find project root and update manifest
    let project_root = find_project_root(".")?;
    let manifest_path = project_root.join(MANIFEST_FILENAME);
    let mut manifest = Manifest::from_file(&manifest_path)?;

    manifest.bhc_platform.snapshot = Some(platform_str.to_string());
    manifest.to_file(&manifest_path)?;

    output.status(
        "Set",
        &format!(
            "BHC Platform to {} ({} packages, BHC {})",
            platform_str,
            snapshot.metadata.package_count,
            snapshot
                .metadata
                .bhc_version
                .as_deref()
                .unwrap_or("unknown"),
        ),
    );

    Ok(0)
}
