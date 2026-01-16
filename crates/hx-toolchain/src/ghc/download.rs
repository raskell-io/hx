//! GHC binary download and installation.
//!
//! Downloads GHC binaries directly from downloads.haskell.org and extracts them
//! to the hx toolchain directory.

use crate::ghc::{
    ghc_archive_filename, ghc_download_url, InstallSource, InstalledGhc, Platform,
    ToolchainManifest, is_valid_version,
};
use futures_util::StreamExt;
use hx_core::{CommandRunner, Error, Result};
use hx_ui::{Progress, Spinner};
use reqwest::Client;
use std::fs::{self, File};
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use tar::Archive;
use tracing::{debug, info, warn};
use xz2::read::XzDecoder;

/// Options for downloading and installing GHC.
#[derive(Debug, Clone)]
pub struct DownloadOptions {
    /// GHC version to install.
    pub version: String,
    /// Target platform (auto-detected if None).
    pub platform: Option<Platform>,
    /// Base toolchain directory.
    pub toolchain_dir: PathBuf,
    /// Whether to set as active after installation.
    pub set_active: bool,
    /// Force reinstall even if already installed.
    pub force: bool,
    /// HTTP timeout in seconds.
    pub timeout: u64,
}

impl Default for DownloadOptions {
    fn default() -> Self {
        Self {
            version: String::new(),
            platform: None,
            toolchain_dir: PathBuf::new(),
            timeout: 300, // 5 minutes for large GHC downloads
            set_active: false,
            force: false,
        }
    }
}

/// Result of a GHC installation.
#[derive(Debug)]
pub struct InstallResult {
    /// The installed GHC entry.
    pub installed: InstalledGhc,
    /// Whether this was a fresh install or already existed.
    pub was_cached: bool,
}

/// Download and install GHC.
pub async fn download_and_install_ghc(options: &DownloadOptions) -> Result<InstallResult> {
    // Validate version format
    if !is_valid_version(&options.version) {
        return Err(Error::config(format!(
            "Invalid GHC version format: {}. Expected format like 9.8.2",
            options.version
        )));
    }

    // Detect platform
    let platform = options.platform.unwrap_or_else(|| {
        Platform::current().expect("Unsupported platform")
    });

    // Check if already installed
    let manifest = ToolchainManifest::load(&options.toolchain_dir)?;
    if !options.force && manifest.is_installed(&options.version) {
        info!("GHC {} is already installed", options.version);
        let installed = manifest.get_ghc(&options.version).unwrap().clone();
        return Ok(InstallResult {
            installed,
            was_cached: true,
        });
    }

    info!(
        "Installing GHC {} for {}",
        options.version,
        platform.display_name()
    );

    // Create directories
    let ghc_dir = options.toolchain_dir.join("ghc");
    let install_dir = ghc_dir.join(&options.version);
    let download_dir = options.toolchain_dir.join("downloads");
    fs::create_dir_all(&download_dir).map_err(|e| Error::Io {
        message: "Failed to create downloads directory".into(),
        path: Some(download_dir.clone()),
        source: e,
    })?;

    // Download the archive
    let archive_name = ghc_archive_filename(&options.version, platform);
    let archive_path = download_dir.join(&archive_name);
    let url = ghc_download_url(&options.version, platform);

    download_ghc_archive(&url, &archive_path, &options.version, options.timeout).await?;

    // Extract the archive
    extract_ghc_archive(&archive_path, &download_dir, &options.version)?;

    // Run configure and make install
    let extracted_dir = download_dir.join(format!("ghc-{}", options.version));
    install_ghc_bindist(&extracted_dir, &install_dir).await?;

    // Verify installation
    verify_ghc_installation(&install_dir, &options.version).await?;

    // Update manifest
    let installed = InstalledGhc::new(&options.version, &install_dir)
        .with_source(InstallSource::Direct);

    let mut manifest = ToolchainManifest::load(&options.toolchain_dir)?;
    manifest.add_ghc(installed.clone());

    if options.set_active {
        manifest.set_active(&options.version)?;
    }

    manifest.save(&options.toolchain_dir)?;

    // Clean up extracted directory (keep archive for potential reinstall)
    let _ = fs::remove_dir_all(&extracted_dir);

    info!("GHC {} installed successfully", options.version);

    Ok(InstallResult {
        installed,
        was_cached: false,
    })
}

/// Download a GHC archive with progress display.
async fn download_ghc_archive(
    url: &str,
    dest: &Path,
    version: &str,
    timeout: u64,
) -> Result<()> {
    // Check if already downloaded
    if dest.exists() {
        debug!("Archive already downloaded: {}", dest.display());
        return Ok(());
    }

    let spinner = Spinner::new(format!("Downloading GHC {}...", version));

    let client = Client::builder()
        .timeout(Duration::from_secs(timeout))
        .build()
        .map_err(|e| Error::config(format!("Failed to create HTTP client: {}", e)))?;

    debug!("Downloading from {}", url);
    let response = client.get(url).send().await.map_err(|e| {
        Error::config(format!("Failed to download GHC {}: {}", version, e))
    })?;

    if !response.status().is_success() {
        spinner.finish_error(format!("Failed to download GHC {}", version));
        return Err(Error::config(format!(
            "GHC {} download failed: HTTP {}. This version may not be available for your platform.",
            version,
            response.status()
        )));
    }

    let total_size = response.content_length().unwrap_or(0);
    spinner.finish_clear();

    // Show progress bar if we know the size
    let progress = if total_size > 0 {
        Some(Progress::new(total_size, format!("Downloading GHC {}", version)))
    } else {
        None
    };

    // Download to temp file first
    let temp_path = dest.with_extension("tmp");
    let mut file = File::create(&temp_path).map_err(|e| Error::Io {
        message: "Failed to create download file".into(),
        path: Some(temp_path.clone()),
        source: e,
    })?;

    let mut stream = response.bytes_stream();
    let mut downloaded: u64 = 0;

    while let Some(chunk) = stream.next().await {
        let chunk = chunk.map_err(|e| {
            Error::config(format!("Download interrupted: {}", e))
        })?;
        file.write_all(&chunk).map_err(|e| Error::Io {
            message: "Failed to write download data".into(),
            path: Some(temp_path.clone()),
            source: e,
        })?;
        downloaded += chunk.len() as u64;
        if let Some(ref pb) = progress {
            pb.set_position(downloaded);
        }
    }

    if let Some(pb) = progress {
        pb.finish(format!("Downloaded GHC {} ({:.1} MB)", version, downloaded as f64 / 1_000_000.0));
    }

    // Rename temp file to final location
    fs::rename(&temp_path, dest).map_err(|e| Error::Io {
        message: "Failed to finalize download".into(),
        path: Some(dest.to_path_buf()),
        source: e,
    })?;

    Ok(())
}

/// Extract a tar.xz archive.
fn extract_ghc_archive(archive_path: &Path, dest_dir: &Path, version: &str) -> Result<()> {
    let spinner = Spinner::new(format!("Extracting GHC {}...", version));

    debug!("Extracting {} to {}", archive_path.display(), dest_dir.display());

    let file = File::open(archive_path).map_err(|e| Error::Io {
        message: "Failed to open archive".into(),
        path: Some(archive_path.to_path_buf()),
        source: e,
    })?;

    let reader = BufReader::new(file);
    let decoder = XzDecoder::new(reader);
    let mut archive = Archive::new(decoder);

    archive.unpack(dest_dir).map_err(|e| Error::Io {
        message: format!("Failed to extract archive: {}", e),
        path: Some(archive_path.to_path_buf()),
        source: e,
    })?;

    spinner.finish_success(format!("Extracted GHC {}", version));
    Ok(())
}

/// Run configure and make install for a GHC bindist.
async fn install_ghc_bindist(extracted_dir: &Path, install_dir: &Path) -> Result<()> {
    let spinner = Spinner::new("Configuring GHC...");

    // Ensure install directory exists
    fs::create_dir_all(install_dir).map_err(|e| Error::Io {
        message: "Failed to create install directory".into(),
        path: Some(install_dir.to_path_buf()),
        source: e,
    })?;

    let runner = CommandRunner::new().with_working_dir(extracted_dir);

    // Run ./configure --prefix=<install_dir>
    debug!(
        "Running configure in {} with prefix {}",
        extracted_dir.display(),
        install_dir.display()
    );

    let configure_script = extracted_dir.join("configure");
    if !configure_script.exists() {
        spinner.finish_error("Configure script not found");
        return Err(Error::config(format!(
            "Configure script not found in {}. The archive may be corrupted.",
            extracted_dir.display()
        )));
    }

    let output = runner
        .run(
            "./configure",
            ["--prefix", install_dir.to_str().unwrap()],
        )
        .await?;

    if !output.success() {
        spinner.finish_error("Configure failed");
        return Err(Error::CommandFailed {
            command: "./configure".into(),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![],
        });
    }

    spinner.finish_success("Configure complete");

    // Run make install
    let spinner = Spinner::new("Installing GHC (this may take a few minutes)...");

    let output = runner
        .run("make", ["install"])
        .await?;

    if !output.success() {
        spinner.finish_error("Installation failed");
        return Err(Error::CommandFailed {
            command: "make install".into(),
            exit_code: Some(output.exit_code),
            stdout: output.stdout,
            stderr: output.stderr,
            fixes: vec![],
        });
    }

    spinner.finish_success("GHC installed");
    Ok(())
}

/// Verify GHC installation by running ghc --version.
async fn verify_ghc_installation(install_dir: &Path, expected_version: &str) -> Result<()> {
    let ghc_path = install_dir.join("bin").join(ghc_binary_name());

    if !ghc_path.exists() {
        return Err(Error::config(format!(
            "GHC binary not found at {}. Installation may have failed.",
            ghc_path.display()
        )));
    }

    let runner = CommandRunner::new();
    let output = runner
        .run(ghc_path.to_str().unwrap(), ["--numeric-version"])
        .await?;

    if !output.success() {
        return Err(Error::config(
            "GHC verification failed: could not get version",
        ));
    }

    let installed_version = output.stdout.trim();
    if installed_version != expected_version {
        warn!(
            "Version mismatch: expected {}, got {}",
            expected_version, installed_version
        );
    }

    debug!("Verified GHC {} at {}", installed_version, ghc_path.display());
    Ok(())
}

/// Get the GHC binary name for the current platform.
fn ghc_binary_name() -> &'static str {
    if cfg!(windows) {
        "ghc.exe"
    } else {
        "ghc"
    }
}

/// Remove an installed GHC version.
pub fn remove_ghc(toolchain_dir: &Path, version: &str) -> Result<bool> {
    let mut manifest = ToolchainManifest::load(toolchain_dir)?;

    if let Some(installed) = manifest.remove_ghc(version) {
        // Remove the installation directory
        if installed.install_path.exists() {
            fs::remove_dir_all(&installed.install_path).map_err(|e| Error::Io {
                message: format!("Failed to remove GHC {}", version),
                path: Some(installed.install_path.clone()),
                source: e,
            })?;
        }

        manifest.save(toolchain_dir)?;
        info!("Removed GHC {}", version);
        Ok(true)
    } else {
        Ok(false)
    }
}

/// List installed GHC versions.
pub fn list_installed(toolchain_dir: &Path) -> Result<Vec<InstalledGhc>> {
    let manifest = ToolchainManifest::load(toolchain_dir)?;
    Ok(manifest.ghc)
}

/// Get the active GHC version.
pub fn get_active(toolchain_dir: &Path) -> Result<Option<InstalledGhc>> {
    let manifest = ToolchainManifest::load(toolchain_dir)?;
    Ok(manifest.active().cloned())
}

/// Set the active GHC version.
pub fn set_active(toolchain_dir: &Path, version: &str) -> Result<()> {
    let mut manifest = ToolchainManifest::load(toolchain_dir)?;
    manifest.set_active(version)?;
    manifest.save(toolchain_dir)?;
    info!("Set GHC {} as active", version);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_download_options_default() {
        let options = DownloadOptions::default();
        assert_eq!(options.timeout, 300);
        assert!(!options.set_active);
        assert!(!options.force);
    }

    #[test]
    fn test_ghc_binary_name() {
        let name = ghc_binary_name();
        if cfg!(windows) {
            assert_eq!(name, "ghc.exe");
        } else {
            assert_eq!(name, "ghc");
        }
    }
}
