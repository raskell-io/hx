//! Toolchain installation.
//!
//! Provides smart installation that tries direct download first, then falls back to ghcup.

use crate::ghc::{self, DownloadOptions, Platform};
use hx_cache::toolchain_dir;
use hx_core::{CommandRunner, Error, Fix, Result};
use hx_ui::Spinner;
use tracing::{info, warn};

/// Install GHC via ghcup.
pub async fn install_ghc(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    let spinner = Spinner::new(format!("Installing GHC {}", version));

    info!("Installing GHC {} via ghcup", version);

    let output = runner.run("ghcup", ["install", "ghc", version]).await?;

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

    let output = runner.run("ghcup", ["install", "cabal", version]).await?;

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

    let output = runner.run("ghcup", ["install", "hls", version]).await?;

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

/// Set GHC version via ghcup (without installing).
pub async fn set_ghc(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    info!("Setting GHC to {} via ghcup", version);

    let output = runner.run("ghcup", ["set", "ghc", version]).await?;

    if output.success() {
        Ok(())
    } else {
        Err(Error::CommandFailed {
            command: format!("ghcup set ghc {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![Fix::with_command(
                format!("Install GHC {} first", version),
                format!("hx toolchain install --ghc {}", version),
            )],
        })
    }
}

/// Set Cabal version via ghcup (without installing).
pub async fn set_cabal(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    info!("Setting Cabal to {} via ghcup", version);

    let output = runner.run("ghcup", ["set", "cabal", version]).await?;

    if output.success() {
        Ok(())
    } else {
        Err(Error::CommandFailed {
            command: format!("ghcup set cabal {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![Fix::with_command(
                format!("Install Cabal {} first", version),
                format!("hx toolchain install --cabal {}", version),
            )],
        })
    }
}

/// Set HLS version via ghcup (without installing).
pub async fn set_hls(version: &str) -> Result<()> {
    let runner = CommandRunner::new();
    info!("Setting HLS to {} via ghcup", version);

    let output = runner.run("ghcup", ["set", "hls", version]).await?;

    if output.success() {
        Ok(())
    } else {
        Err(Error::CommandFailed {
            command: format!("ghcup set hls {}", version),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![Fix::with_command(
                format!("Install HLS {} first", version),
                format!("hx toolchain install --hls {}", version),
            )],
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

/// Smart GHC installation strategy.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum InstallStrategy {
    /// Try direct download first, fall back to ghcup.
    #[default]
    Smart,
    /// Only use direct download (fail if not available).
    Direct,
    /// Only use ghcup.
    Ghcup,
}

/// Options for smart GHC installation.
#[derive(Debug, Clone)]
pub struct SmartInstallOptions {
    /// GHC version to install.
    pub version: String,
    /// Installation strategy.
    pub strategy: InstallStrategy,
    /// Whether to set as active after installation.
    pub set_active: bool,
    /// Force reinstall even if already installed.
    pub force: bool,
}

impl SmartInstallOptions {
    /// Create new options for a version.
    pub fn new(version: impl Into<String>) -> Self {
        Self {
            version: version.into(),
            strategy: InstallStrategy::default(),
            set_active: false,
            force: false,
        }
    }

    /// Set the installation strategy.
    pub fn with_strategy(mut self, strategy: InstallStrategy) -> Self {
        self.strategy = strategy;
        self
    }

    /// Set as active after installation.
    pub fn with_set_active(mut self, set_active: bool) -> Self {
        self.set_active = set_active;
        self
    }

    /// Force reinstall.
    pub fn with_force(mut self, force: bool) -> Self {
        self.force = force;
        self
    }
}

/// Install GHC using smart strategy: direct download first, fallback to ghcup.
///
/// This is the preferred method for installing GHC as it:
/// 1. Tries direct download from downloads.haskell.org (faster, no ghcup dependency)
/// 2. Falls back to ghcup if direct download fails
///
/// Use `InstallStrategy::Direct` to disable ghcup fallback.
/// Use `InstallStrategy::Ghcup` to skip direct download.
pub async fn install_ghc_smart(options: &SmartInstallOptions) -> Result<()> {
    match options.strategy {
        InstallStrategy::Direct => install_ghc_direct(options).await,
        InstallStrategy::Ghcup => install_ghc(&options.version).await,
        InstallStrategy::Smart => {
            // Check if platform is supported for direct download
            if Platform::current().is_none() {
                info!("Platform not supported for direct download, using ghcup");
                return install_ghc(&options.version).await;
            }

            // Try direct download first
            match install_ghc_direct(options).await {
                Ok(()) => Ok(()),
                Err(e) => {
                    warn!(
                        "Direct GHC installation failed, falling back to ghcup: {}",
                        e
                    );
                    install_ghc(&options.version).await
                }
            }
        }
    }
}

/// Install GHC via direct download.
async fn install_ghc_direct(options: &SmartInstallOptions) -> Result<()> {
    let tc_dir = toolchain_dir()?;

    let download_options = DownloadOptions {
        version: options.version.clone(),
        platform: Platform::current(),
        toolchain_dir: tc_dir,
        set_active: options.set_active,
        force: options.force,
        ..Default::default()
    };

    let result = ghc::download_and_install_ghc(&download_options).await?;

    if result.was_cached {
        info!("GHC {} was already installed", options.version);
    } else {
        info!("GHC {} installed successfully", options.version);
    }

    Ok(())
}
