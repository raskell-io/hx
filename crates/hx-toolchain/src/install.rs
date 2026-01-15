//! Toolchain installation via ghcup.

use hx_core::{CommandRunner, Error, Result};
use hx_ui::Spinner;
use tracing::info;

/// Install GHC via ghcup.
pub async fn install_ghc(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    let spinner = Spinner::new(format!("Installing GHC {}", version));

    info!("Installing GHC {} via ghcup", version);

    let output = runner
        .run("ghcup", ["install", "ghc", version])
        .await?;

    if output.success() {
        // Set as default
        let _ = runner.run("ghcup", ["set", "ghc", version]).await;
        spinner.finish_success(format!("GHC {} installed", version));
        Ok(())
    } else {
        spinner.finish_error(format!("Failed to install GHC {}", version));
        Err(Error::CommandFailed {
            command: format!("ghcup install ghc {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![],
        })
    }
}

/// Install Cabal via ghcup.
pub async fn install_cabal(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    let spinner = Spinner::new(format!("Installing Cabal {}", version));

    info!("Installing Cabal {} via ghcup", version);

    let output = runner
        .run("ghcup", ["install", "cabal", version])
        .await?;

    if output.success() {
        let _ = runner.run("ghcup", ["set", "cabal", version]).await;
        spinner.finish_success(format!("Cabal {} installed", version));
        Ok(())
    } else {
        spinner.finish_error(format!("Failed to install Cabal {}", version));
        Err(Error::CommandFailed {
            command: format!("ghcup install cabal {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![],
        })
    }
}

/// Install HLS via ghcup.
pub async fn install_hls(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    let spinner = Spinner::new(format!("Installing HLS {}", version));

    info!("Installing HLS {} via ghcup", version);

    let output = runner
        .run("ghcup", ["install", "hls", version])
        .await?;

    if output.success() {
        let _ = runner.run("ghcup", ["set", "hls", version]).await;
        spinner.finish_success(format!("HLS {} installed", version));
        Ok(())
    } else {
        spinner.finish_error(format!("Failed to install HLS {}", version));
        Err(Error::CommandFailed {
            command: format!("ghcup install hls {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![],
        })
    }
}

/// Get the ghcup install command for the current platform.
pub fn ghcup_install_command() -> &'static str {
    if cfg!(windows) {
        "Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }"
    } else {
        "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    }
}
