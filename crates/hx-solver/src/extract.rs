//! Package source extraction from tarballs.
//!
//! This module handles extracting downloaded Hackage tarballs and parsing their
//! .cabal files to get build information.

use crate::cabal::{PackageBuildInfo, parse_cabal_full};
use crate::fetch::FetchResult;
use crate::version::Version;
use flate2::read::GzDecoder;
use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};
use tar::Archive;
use thiserror::Error;
use tokio::sync::Semaphore;
use tracing::{debug, info, warn};

/// Error type for extraction operations.
#[derive(Debug, Error)]
pub enum ExtractError {
    #[error("failed to open tarball: {path}: {source}")]
    OpenTarball {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("failed to read tarball: {path}: {source}")]
    ReadTarball {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("failed to extract to {path}: {source}")]
    ExtractFailed {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("no .cabal file found in package {name}-{version}")]
    NoCabalFile { name: String, version: String },

    #[error("failed to read .cabal file: {path}: {source}")]
    ReadCabalFile {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("failed to create directory {path}: {source}")]
    CreateDir {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("package already extracted: {path}")]
    AlreadyExtracted { path: PathBuf },
}

/// Result of extracting a package.
#[derive(Debug, Clone)]
pub struct ExtractedPackage {
    /// Package name
    pub name: String,
    /// Package version
    pub version: Version,
    /// Path to extracted source directory
    pub source_dir: PathBuf,
    /// Path to the .cabal file
    pub cabal_file: PathBuf,
    /// Parsed build information
    pub build_info: PackageBuildInfo,
}

impl ExtractedPackage {
    /// Check if this package can be built natively.
    pub fn can_build_native(&self) -> bool {
        self.build_info.is_simple_build() && !self.build_info.needs_unsupported_preprocessors()
    }

    /// Get the reason why this package can't be built natively.
    pub fn skip_reason(&self) -> Option<String> {
        if !self.build_info.is_simple_build() {
            return Some(format!(
                "requires {:?} build type",
                self.build_info.build_type
            ));
        }
        if self.build_info.needs_unsupported_preprocessors() {
            return Some("requires unsupported preprocessors (c2hs/cpphs)".to_string());
        }
        None
    }
}

/// Options for extraction.
#[derive(Debug, Clone)]
pub struct ExtractOptions {
    /// Number of parallel extraction jobs
    pub jobs: usize,
    /// Force re-extraction even if already extracted
    pub force: bool,
}

impl Default for ExtractOptions {
    fn default() -> Self {
        Self {
            jobs: 4,
            force: false,
        }
    }
}

/// Extract a single package tarball.
///
/// # Arguments
/// * `tarball` - Path to the .tar.gz file
/// * `cache_dir` - Base cache directory (sources will go in `cache_dir/sources/`)
/// * `force` - If true, re-extract even if already exists
///
/// # Returns
/// The extracted package information including parsed .cabal contents.
pub fn extract_package(
    tarball: &Path,
    cache_dir: &Path,
    force: bool,
) -> Result<ExtractedPackage, ExtractError> {
    // Parse package name and version from tarball filename
    // Format: package-version.tar.gz
    let filename = tarball.file_stem().and_then(|s| s.to_str()).unwrap_or("");

    // Remove .tar if present (for .tar.gz files, file_stem gives us foo-1.0.tar)
    let filename = filename.strip_suffix(".tar").unwrap_or(filename);

    let (name, version_str) = parse_package_filename(filename)?;
    let version: Version = version_str.parse().unwrap_or_default();

    let sources_dir = cache_dir.join("sources");
    let package_dir = sources_dir.join(format!("{}-{}", name, version_str));

    // Check if already extracted
    if package_dir.exists() && !force {
        debug!("Package already extracted: {}", package_dir.display());
        // Try to load existing extraction
        return load_extracted_package(&package_dir, &name, &version);
    }

    // Create sources directory
    fs::create_dir_all(&sources_dir).map_err(|e| ExtractError::CreateDir {
        path: sources_dir.clone(),
        source: e,
    })?;

    // Remove existing directory if force
    if package_dir.exists() && force {
        fs::remove_dir_all(&package_dir).map_err(|e| ExtractError::ExtractFailed {
            path: package_dir.clone(),
            source: e,
        })?;
    }

    info!("Extracting {}-{}", name, version_str);

    // Open and decompress tarball
    let file = File::open(tarball).map_err(|e| ExtractError::OpenTarball {
        path: tarball.to_path_buf(),
        source: e,
    })?;

    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);

    // Extract to sources directory
    // Hackage tarballs contain a top-level directory named package-version/
    archive
        .unpack(&sources_dir)
        .map_err(|e| ExtractError::ExtractFailed {
            path: sources_dir.clone(),
            source: e,
        })?;

    // Load and parse the extracted package
    load_extracted_package(&package_dir, &name, &version)
}

/// Load an already-extracted package.
fn load_extracted_package(
    package_dir: &Path,
    name: &str,
    version: &Version,
) -> Result<ExtractedPackage, ExtractError> {
    // Find the .cabal file
    let cabal_file = find_cabal_file(package_dir).ok_or_else(|| ExtractError::NoCabalFile {
        name: name.to_string(),
        version: version.to_string(),
    })?;

    // Read and parse the .cabal file
    let cabal_content =
        fs::read_to_string(&cabal_file).map_err(|e| ExtractError::ReadCabalFile {
            path: cabal_file.clone(),
            source: e,
        })?;

    let build_info = parse_cabal_full(&cabal_content);

    Ok(ExtractedPackage {
        name: name.to_string(),
        version: version.clone(),
        source_dir: package_dir.to_path_buf(),
        cabal_file,
        build_info,
    })
}

/// Find the .cabal file in a package directory.
fn find_cabal_file(dir: &Path) -> Option<PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "cabal") {
            return Some(path);
        }
    }
    None
}

/// Parse package name and version from filename like "text-2.1.1".
fn parse_package_filename(filename: &str) -> Result<(String, String), ExtractError> {
    // Find the last hyphen that precedes a digit (version separator)
    let mut split_pos = None;
    let chars: Vec<char> = filename.chars().collect();

    for i in (0..chars.len()).rev() {
        if chars[i] == '-' {
            // Check if next char is a digit
            if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                split_pos = Some(i);
                break;
            }
        }
    }

    match split_pos {
        Some(pos) => {
            let name = filename[..pos].to_string();
            let version = filename[pos + 1..].to_string();
            Ok((name, version))
        }
        None => {
            // No version found, use filename as name
            Ok((filename.to_string(), "0".to_string()))
        }
    }
}

/// Extract multiple packages in parallel.
pub async fn extract_all_packages(
    fetch_results: &[FetchResult],
    cache_dir: &Path,
    options: &ExtractOptions,
) -> Result<Vec<ExtractedPackage>, ExtractError> {
    let semaphore = std::sync::Arc::new(Semaphore::new(options.jobs));
    let mut handles = Vec::new();

    for result in fetch_results {
        let tarball = result.path.clone();
        let cache_dir = cache_dir.to_path_buf();
        let force = options.force;
        let permit = semaphore.clone().acquire_owned().await.unwrap();

        let handle = tokio::task::spawn_blocking(move || {
            let result = extract_package(&tarball, &cache_dir, force);
            drop(permit);
            result
        });

        handles.push(handle);
    }

    let mut extracted = Vec::new();
    for handle in handles {
        match handle.await {
            Ok(Ok(pkg)) => {
                extracted.push(pkg);
            }
            Ok(Err(e)) => {
                warn!("Failed to extract package: {}", e);
            }
            Err(e) => {
                warn!("Extraction task panicked: {}", e);
            }
        }
    }

    Ok(extracted)
}

/// Get the sources directory for a cache.
pub fn sources_dir(cache_dir: &Path) -> PathBuf {
    cache_dir.join("sources")
}

/// Check if a package is already extracted.
pub fn is_extracted(cache_dir: &Path, name: &str, version: &str) -> bool {
    let package_dir = sources_dir(cache_dir).join(format!("{}-{}", name, version));
    package_dir.exists() && find_cabal_file(&package_dir).is_some()
}

/// Clear all extracted sources.
pub fn clear_sources(cache_dir: &Path) -> io::Result<()> {
    let dir = sources_dir(cache_dir);
    if dir.exists() {
        fs::remove_dir_all(&dir)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_package_filename() {
        let (name, ver) = parse_package_filename("text-2.1.1").unwrap();
        assert_eq!(name, "text");
        assert_eq!(ver, "2.1.1");

        let (name, ver) = parse_package_filename("aeson-pretty-0.8.10").unwrap();
        assert_eq!(name, "aeson-pretty");
        assert_eq!(ver, "0.8.10");

        let (name, ver) = parse_package_filename("base64-bytestring-1.2.1.0").unwrap();
        assert_eq!(name, "base64-bytestring");
        assert_eq!(ver, "1.2.1.0");

        let (name, ver) = parse_package_filename("HTTP-4000.4.1").unwrap();
        assert_eq!(name, "HTTP");
        assert_eq!(ver, "4000.4.1");
    }

    #[test]
    fn test_sources_dir() {
        let cache = PathBuf::from("/home/user/.cache/hx");
        let dir = sources_dir(&cache);
        assert_eq!(dir, PathBuf::from("/home/user/.cache/hx/sources"));
    }
}
