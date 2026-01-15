//! Cache management for hx.
//!
//! This crate handles:
//! - Global cache directories
//! - Project-local cache (.hx/)
//! - Cache keys and artifact storage

use directories::ProjectDirs;
use hx_core::error::{Error, Result};
use std::path::{Path, PathBuf};
use tracing::debug;

/// Get the global cache directory.
///
/// - Linux: `~/.cache/hx`
/// - macOS: `~/Library/Caches/hx`
/// - Windows: `%LOCALAPPDATA%\hx\cache`
pub fn global_cache_dir() -> Result<PathBuf> {
    let dirs = ProjectDirs::from("io", "raskell", "hx")
        .ok_or_else(|| Error::config("could not determine home directory for cache"))?;
    Ok(dirs.cache_dir().to_path_buf())
}

/// Get the Cabal store directory within the global cache.
pub fn cabal_store_dir() -> Result<PathBuf> {
    Ok(global_cache_dir()?.join("cabal").join("store"))
}

/// Get the global config directory.
///
/// - Linux: `~/.config/hx`
/// - macOS: `~/Library/Application Support/hx`
/// - Windows: `%APPDATA%\hx\config`
pub fn global_config_dir() -> Result<PathBuf> {
    let dirs = ProjectDirs::from("io", "raskell", "hx")
        .ok_or_else(|| Error::config("could not determine home directory for config"))?;
    Ok(dirs.config_dir().to_path_buf())
}

/// Ensure a directory exists.
pub fn ensure_dir(path: &PathBuf) -> Result<()> {
    if !path.exists() {
        debug!("Creating directory: {}", path.display());
        std::fs::create_dir_all(path).map_err(|e| Error::Io {
            message: format!("failed to create directory: {}", path.display()),
            path: Some(path.clone()),
            source: e,
        })?;
    }
    Ok(())
}

/// Clean the global cache.
pub fn clean_global_cache() -> Result<()> {
    let cache_dir = global_cache_dir()?;
    if cache_dir.exists() {
        debug!("Removing global cache: {}", cache_dir.display());
        std::fs::remove_dir_all(&cache_dir).map_err(|e| Error::Io {
            message: "failed to remove global cache".to_string(),
            path: Some(cache_dir),
            source: e,
        })?;
    }
    Ok(())
}

/// Clean a project's local cache.
pub fn clean_project_cache(project_root: &Path) -> Result<()> {
    let cache_dir = project_root.join(".hx");
    if cache_dir.exists() {
        debug!("Removing project cache: {}", cache_dir.display());
        std::fs::remove_dir_all(&cache_dir).map_err(|e| Error::Io {
            message: "failed to remove project cache".to_string(),
            path: Some(cache_dir),
            source: e,
        })?;
    }
    Ok(())
}
