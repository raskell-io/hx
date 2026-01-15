//! Toolchain command implementation.

use crate::cli::ToolchainCommands;
use anyhow::Result;
use hx_toolchain::{install, Toolchain};
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
        output.list_item("cabal", &format!("{} (version unknown)", Style::warning("?")));
    } else {
        output.list_item("cabal", &format!("{} not found", Style::error("✗")));
    }

    // GHCup
    if let Some(version) = toolchain.ghcup.status.version() {
        output.list_item("ghcup", &format!("{} {}", Style::success("✓"), version));
    } else if toolchain.ghcup.status.is_found() {
        output.list_item("ghcup", &format!("{} (version unknown)", Style::warning("?")));
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

    if success {
        Ok(0)
    } else {
        Ok(4)
    }
}

async fn use_profile(profile: &str, output: &Output) -> Result<i32> {
    output.warn(&format!(
        "hx toolchain use is not yet fully implemented. Profile: {}",
        profile
    ));
    output.info("For now, use ghcup directly: ghcup set ghc <version>");
    Ok(1)
}
