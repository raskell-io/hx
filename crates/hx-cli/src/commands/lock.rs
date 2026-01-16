//! Lock command implementation.

use anyhow::Result;
use hx_cabal::freeze;
use hx_config::{LOCKFILE_FILENAME, Project, find_project_root};
use hx_lock::{Lockfile, parse_freeze_file};
use hx_toolchain::Toolchain;
use hx_ui::{Output, Spinner};

/// Run the lock command.
pub async fn run(output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Locking", project.name());

    // Update package index
    freeze::update(output.is_verbose()).await?;

    // Run cabal freeze
    let build_dir = project.cabal_build_dir();
    let freeze_content = freeze::freeze(&project.root, &build_dir).await?;

    // Create lockfile
    let spinner = Spinner::new("Creating lockfile...");

    let mut lockfile = Lockfile::new();

    // Set toolchain versions from detected tools
    let toolchain = Toolchain::detect().await;
    lockfile.set_toolchain(
        toolchain.ghc.status.version().map(|v| v.to_string()),
        toolchain.cabal.status.version().map(|v| v.to_string()),
    );

    // Parse freeze file and add packages
    let packages = parse_freeze_file(&freeze_content);
    for pkg in packages {
        lockfile.add_package(pkg);
    }

    // Calculate fingerprint
    let fingerprint = lockfile.fingerprint();
    lockfile.plan.hash = Some(fingerprint);

    // Detect platform
    lockfile.plan.platform = Some(detect_platform());

    // Write lockfile
    let lockfile_path = project.lockfile_path();
    lockfile.to_file(&lockfile_path)?;

    spinner.finish_success(format!(
        "Created {} with {} packages",
        LOCKFILE_FILENAME,
        lockfile.packages.len()
    ));

    output.info(&format!("Lockfile: {}", lockfile_path.display()));

    Ok(0)
}

/// Run the sync command.
pub async fn sync(force: bool, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Syncing", project.name());

    // Check if lockfile exists
    let lockfile_path = project.lockfile_path();
    if !lockfile_path.exists() {
        output.error(&format!("{} not found", LOCKFILE_FILENAME));
        output.info("Run `hx lock` to create a lockfile first");
        return Ok(3); // Config error
    }

    // Load lockfile
    let lockfile = Lockfile::from_file(&lockfile_path)?;

    // Verify toolchain if not forcing
    if !force {
        let toolchain = Toolchain::detect().await;

        if let (Some(lock_ghc), Some(detected_ghc)) =
            (&lockfile.toolchain.ghc, toolchain.ghc.status.version())
            && lock_ghc != &detected_ghc.to_string()
        {
            output.warn(&format!(
                "GHC version mismatch: lock has {}, detected {}",
                lock_ghc, detected_ghc
            ));
            output.info("Run `hx toolchain install` to install the correct version");
            output.info("Or run `hx sync --force` to override");
        }
    }

    // Build with locked dependencies
    // For now, just run a regular build since we have the freeze file
    let build_dir = project.cabal_build_dir();

    let options = hx_cabal::BuildOptions {
        release: false,
        jobs: None,
        target: None,
        package: None,
        verbose: output.is_verbose(),
    };

    match hx_cabal::build::build(&project.root, &build_dir, &options, output).await {
        Ok(_) => {
            output.success_summary(
                "Synced with locked dependencies",
                std::time::Duration::from_secs(0),
            );
            Ok(0)
        }
        Err(e) => {
            output.print_error(&e);
            Ok(5)
        }
    }
}

fn detect_platform() -> String {
    let arch = std::env::consts::ARCH;
    let os = std::env::consts::OS;
    format!("{}-{}", arch, os)
}
