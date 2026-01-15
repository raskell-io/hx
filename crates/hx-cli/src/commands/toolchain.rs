//! Toolchain command implementation.

use crate::cli::ToolchainCommands;
use anyhow::Result;
use hx_config::{Manifest, find_project_root};
use hx_toolchain::{Toolchain, install};
use hx_ui::{Output, Style};

/// Run a toolchain subcommand.
pub async fn run(command: ToolchainCommands, output: &Output) -> Result<i32> {
    match command {
        ToolchainCommands::Status => status(output).await,
        ToolchainCommands::Install { ghc, cabal, hls } => {
            install_components(ghc, cabal, hls, output).await
        }
        ToolchainCommands::Use { profile } => use_profile(&profile, output).await,
    }
}

async fn status(output: &Output) -> Result<i32> {
    output.status("Detecting", "toolchain");

    let toolchain = Toolchain::detect().await;

    output.header("Toolchain Status");

    // GHC
    if let Some(version) = toolchain.ghc.status.version() {
        output.list_item("ghc", &format!("{} {}", Style::success("✓"), version));
    } else if toolchain.ghc.status.is_found() {
        output.list_item("ghc", &format!("{} (version unknown)", Style::warning("?")));
    } else {
        output.list_item("ghc", &format!("{} not found", Style::error("✗")));
    }

    // Cabal
    if let Some(version) = toolchain.cabal.status.version() {
        output.list_item("cabal", &format!("{} {}", Style::success("✓"), version));
    } else if toolchain.cabal.status.is_found() {
        output.list_item(
            "cabal",
            &format!("{} (version unknown)", Style::warning("?")),
        );
    } else {
        output.list_item("cabal", &format!("{} not found", Style::error("✗")));
    }

    // GHCup
    if let Some(version) = toolchain.ghcup.status.version() {
        output.list_item("ghcup", &format!("{} {}", Style::success("✓"), version));
    } else if toolchain.ghcup.status.is_found() {
        output.list_item(
            "ghcup",
            &format!("{} (version unknown)", Style::warning("?")),
        );
    } else {
        output.list_item("ghcup", &format!("{} not found", Style::warning("○")));
    }

    // HLS
    if let Some(version) = toolchain.hls.status.version() {
        output.list_item("hls", &format!("{} {}", Style::success("✓"), version));
    } else if toolchain.hls.status.is_found() {
        output.list_item("hls", &format!("{} (version unknown)", Style::warning("?")));
    } else {
        output.list_item("hls", &format!("{} not found", Style::warning("○")));
    }

    if toolchain.is_complete() {
        Ok(0)
    } else {
        Ok(4) // Toolchain error
    }
}

async fn install_components(
    ghc: Option<String>,
    cabal: Option<String>,
    hls: Option<String>,
    output: &Output,
) -> Result<i32> {
    // Check if ghcup is available
    let toolchain = Toolchain::detect().await;
    if !toolchain.has_ghcup() {
        output.error("ghcup is required to install toolchain components");
        output.info(&format!(
            "Install ghcup: {}",
            install::ghcup_install_command()
        ));
        return Ok(4);
    }

    let mut success = true;

    if let Some(ref version) = ghc {
        output.status("Installing", &format!("GHC {}", version));
        if let Err(e) = install::install_ghc(version).await {
            output.print_error(&e);
            success = false;
        }
    }

    if let Some(ref version) = cabal {
        output.status("Installing", &format!("Cabal {}", version));
        if let Err(e) = install::install_cabal(version).await {
            output.print_error(&e);
            success = false;
        }
    }

    if let Some(ref version) = hls {
        output.status("Installing", &format!("HLS {}", version));
        if let Err(e) = install::install_hls(version).await {
            output.print_error(&e);
            success = false;
        }
    }

    // If no specific versions requested, show current status
    if ghc.is_none() && cabal.is_none() && hls.is_none() {
        output.warn("No versions specified. Use --ghc, --cabal, or --hls to specify versions.");
        output.info("Example: hx toolchain install --ghc 9.8.2");
        return Ok(2);
    }

    if success { Ok(0) } else { Ok(4) }
}

async fn use_profile(profile: &str, output: &Output) -> Result<i32> {
    // Check if ghcup is available
    let toolchain = Toolchain::detect().await;
    if !toolchain.has_ghcup() {
        output.error("ghcup is required to switch toolchain versions");
        output.info(&format!(
            "Install ghcup: {}",
            install::ghcup_install_command()
        ));
        return Ok(4);
    }

    match profile {
        "project" => use_project_toolchain(output).await,
        version => {
            // Assume it's a GHC version
            output.status("Switching", &format!("GHC to {}", version));
            if let Err(e) = install::set_ghc(version).await {
                output.print_error(&e);
                return Ok(4);
            }
            output.status("Done", &format!("Now using GHC {}", version));
            Ok(0)
        }
    }
}

async fn use_project_toolchain(output: &Output) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            output.info("Run `hx init` to create a new project, or specify a version directly:");
            output.info("  hx toolchain use 9.8.2");
            return Ok(3);
        }
    };

    // Load manifest
    let manifest_path = project_root.join("hx.toml");
    let manifest = match Manifest::from_file(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            output.error(&format!("Failed to read hx.toml: {}", e));
            return Ok(3);
        }
    };

    let mut success = true;

    // Set GHC if specified
    if let Some(ref ghc_version) = manifest.toolchain.ghc {
        output.status("Switching", &format!("GHC to {}", ghc_version));
        if let Err(e) = install::set_ghc(ghc_version).await {
            output.print_error(&e);
            success = false;
        } else {
            output.list_item("ghc", ghc_version);
        }
    }

    // Set Cabal if specified
    if let Some(ref cabal_version) = manifest.toolchain.cabal {
        output.status("Switching", &format!("Cabal to {}", cabal_version));
        if let Err(e) = install::set_cabal(cabal_version).await {
            output.print_error(&e);
            success = false;
        } else {
            output.list_item("cabal", cabal_version);
        }
    }

    // Set HLS if specified
    if let Some(ref hls_version) = manifest.toolchain.hls {
        output.status("Switching", &format!("HLS to {}", hls_version));
        if let Err(e) = install::set_hls(hls_version).await {
            output.print_error(&e);
            success = false;
        } else {
            output.list_item("hls", hls_version);
        }
    }

    // Check if any toolchain was specified
    if manifest.toolchain.ghc.is_none()
        && manifest.toolchain.cabal.is_none()
        && manifest.toolchain.hls.is_none()
    {
        output.warn("No toolchain versions specified in hx.toml");
        output.info("Add a [toolchain] section to your hx.toml:");
        output.info("  [toolchain]");
        output.info("  ghc = \"9.8.2\"");
        output.info("  cabal = \"3.12.1.0\"");
        return Ok(0);
    }

    if success {
        output.status("Done", "Toolchain configured from project");
        Ok(0)
    } else {
        Ok(4)
    }
}
