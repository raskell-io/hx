//! BHC Platform command implementation.

use anyhow::Result;
use hx_config::{MANIFEST_FILENAME, Manifest, find_project_root};
use hx_solver::bhc_platform::{self, SnapshotDiff};
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

/// Compare two BHC Platform snapshots.
pub async fn diff(old_str: &str, new_str: &str, output: &Output) -> Result<i32> {
    let old_id = match SnapshotId::parse(old_str) {
        Ok(id) => id,
        Err(e) => {
            output.error(&format!("Invalid platform identifier '{}': {}", old_str, e));
            return Ok(1);
        }
    };

    let new_id = match SnapshotId::parse(new_str) {
        Ok(id) => id,
        Err(e) => {
            output.error(&format!("Invalid platform identifier '{}': {}", new_str, e));
            return Ok(1);
        }
    };

    let old_snap = match bhc_platform::load_bhc_platform(&old_id) {
        Ok(snap) => snap,
        Err(e) => {
            output.error(&format!("Failed to load platform '{}': {}", old_str, e));
            return Ok(1);
        }
    };

    let new_snap = match bhc_platform::load_bhc_platform(&new_id) {
        Ok(snap) => snap,
        Err(e) => {
            output.error(&format!("Failed to load platform '{}': {}", new_str, e));
            return Ok(1);
        }
    };

    let diff = SnapshotDiff::compute(&old_snap, &new_snap);

    output.status("Diff", &format!("{} -> {}", old_str, new_str));
    output.info("");

    if !diff.added.is_empty() {
        output.info(&format!("Added ({}):", diff.added.len()));
        for (name, version) in &diff.added {
            output.info(&format!("  + {} {}", name, version));
        }
        output.info("");
    }

    if !diff.removed.is_empty() {
        output.info(&format!("Removed ({}):", diff.removed.len()));
        for (name, version) in &diff.removed {
            output.info(&format!("  - {} {}", name, version));
        }
        output.info("");
    }

    if !diff.upgraded.is_empty() {
        output.info(&format!("Upgraded ({}):", diff.upgraded.len()));
        for (name, old_ver, new_ver) in &diff.upgraded {
            output.info(&format!("  ~ {} {} -> {}", name, old_ver, new_ver));
        }
        output.info("");
    }

    if !diff.downgraded.is_empty() {
        output.info(&format!("Downgraded ({}):", diff.downgraded.len()));
        for (name, old_ver, new_ver) in &diff.downgraded {
            output.info(&format!("  ~ {} {} -> {}", name, old_ver, new_ver));
        }
        output.info("");
    }

    if diff.added.is_empty()
        && diff.removed.is_empty()
        && diff.upgraded.is_empty()
        && diff.downgraded.is_empty()
    {
        output.info("No differences found.");
    }

    Ok(0)
}

/// Fetch latest snapshot index from the registry.
pub async fn update(output: &Output) -> Result<i32> {
    output.status("Fetching", "BHC Platform snapshot index...");

    match bhc_platform::load_or_fetch_registry().await {
        Ok(registry) => {
            output.info(&format!(
                "{} snapshots available (updated: {})",
                registry.snapshots.len(),
                registry.updated,
            ));
            output.info("");

            for entry in &registry.snapshots {
                output.info(&format!(
                    "  {} (BHC {}, GHC-compat {}, {} packages)",
                    entry.id, entry.bhc_version, entry.ghc_compat, entry.package_count,
                ));
            }

            Ok(0)
        }
        Err(e) => {
            output.error(&format!("Failed to fetch snapshot registry: {}", e));
            output.info("hint: Check your network connection or try again later.");
            Ok(1)
        }
    }
}
