//! Project detection and context.

use crate::{CACHE_DIR_NAME, LOCKFILE_FILENAME, MANIFEST_FILENAME, Manifest};
use hx_core::error::{Error, Fix};
use std::path::{Path, PathBuf};
use tracing::debug;

/// A detected hx project.
#[derive(Debug, Clone)]
pub struct Project {
    /// Path to the project root (containing hx.toml)
    pub root: PathBuf,
    /// Parsed manifest
    pub manifest: Manifest,
    /// Path to the .cabal file, if found
    pub cabal_file: Option<PathBuf>,
    /// Whether cabal.project exists
    pub has_cabal_project: bool,
}

impl Project {
    /// Load a project from a directory.
    pub fn load(root: impl AsRef<Path>) -> Result<Self, Error> {
        let root = root.as_ref().to_path_buf();
        let manifest_path = root.join(MANIFEST_FILENAME);

        let manifest = Manifest::from_file(&manifest_path).map_err(|e| Error::Config {
            message: format!("failed to load manifest: {}", e),
            path: Some(manifest_path.clone()),
            source: Some(Box::new(e)),
            fixes: vec![Fix::with_command("Create a new project", "hx init")],
        })?;

        let cabal_file = find_cabal_file(&root);
        let has_cabal_project = root.join("cabal.project").exists();

        Ok(Self {
            root,
            manifest,
            cabal_file,
            has_cabal_project,
        })
    }

    /// Get the path to the hx.toml manifest.
    pub fn manifest_path(&self) -> PathBuf {
        self.root.join(MANIFEST_FILENAME)
    }

    /// Get the path to the hx.lock file.
    pub fn lockfile_path(&self) -> PathBuf {
        self.root.join(LOCKFILE_FILENAME)
    }

    /// Get the path to the .hx cache directory.
    pub fn cache_dir(&self) -> PathBuf {
        self.root.join(CACHE_DIR_NAME)
    }

    /// Get the cabal build directory.
    pub fn cabal_build_dir(&self) -> PathBuf {
        self.cache_dir().join("cabal").join("dist-newstyle")
    }

    /// Check if the lockfile exists.
    pub fn has_lockfile(&self) -> bool {
        self.lockfile_path().exists()
    }

    /// Get the project name.
    pub fn name(&self) -> &str {
        &self.manifest.project.name
    }
}

/// Find the project root by searching upward for hx.toml.
pub fn find_project_root(start: impl AsRef<Path>) -> Result<PathBuf, Error> {
    let start = start.as_ref();
    let start = if start.is_relative() {
        std::env::current_dir()
            .map_err(|e| Error::Io {
                message: "failed to get current directory".to_string(),
                path: None,
                source: e,
            })?
            .join(start)
    } else {
        start.to_path_buf()
    };

    let mut current = start.as_path();
    let mut searched = Vec::new();

    loop {
        debug!("Checking for project root at: {}", current.display());
        searched.push(current.to_path_buf());

        let manifest_path = current.join(MANIFEST_FILENAME);
        if manifest_path.exists() {
            debug!("Found project root: {}", current.display());
            return Ok(current.to_path_buf());
        }

        // Also check for .cabal file as a fallback
        if find_cabal_file(current).is_some() {
            debug!("Found .cabal file at {} (no hx.toml)", current.display());
            // Return this directory but note there's no hx.toml
            // The caller can decide whether to create one
        }

        match current.parent() {
            Some(parent) => current = parent,
            None => break,
        }
    }

    Err(Error::ProjectNotFound {
        searched,
        fixes: vec![
            Fix::with_command("Create a new project", "hx init"),
            Fix::new("Run from within a project directory containing hx.toml"),
        ],
    })
}

/// Find a .cabal file in the given directory.
fn find_cabal_file(dir: &Path) -> Option<PathBuf> {
    let entries = std::fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "cabal") {
            return Some(path);
        }
    }
    None
}

/// Check if a directory looks like a Haskell project.
pub fn is_haskell_project(dir: &Path) -> bool {
    // Has hx.toml
    if dir.join(MANIFEST_FILENAME).exists() {
        return true;
    }

    // Has .cabal file
    if find_cabal_file(dir).is_some() {
        return true;
    }

    // Has cabal.project
    if dir.join("cabal.project").exists() {
        return true;
    }

    // Has stack.yaml (we don't support stack yet, but it's a Haskell project)
    if dir.join("stack.yaml").exists() {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_find_project_root() {
        let dir = tempdir().unwrap();
        let manifest = dir.path().join(MANIFEST_FILENAME);
        fs::write(
            &manifest,
            r#"
[project]
name = "test"
"#,
        )
        .unwrap();

        let root = find_project_root(dir.path()).unwrap();
        assert_eq!(root, dir.path());
    }

    #[test]
    fn test_find_project_root_nested() {
        let dir = tempdir().unwrap();
        let manifest = dir.path().join(MANIFEST_FILENAME);
        fs::write(
            &manifest,
            r#"
[project]
name = "test"
"#,
        )
        .unwrap();

        let subdir = dir.path().join("src").join("deep");
        fs::create_dir_all(&subdir).unwrap();

        let root = find_project_root(&subdir).unwrap();
        assert_eq!(root, dir.path());
    }

    #[test]
    fn test_project_not_found() {
        let dir = tempdir().unwrap();
        let result = find_project_root(dir.path());
        assert!(result.is_err());
    }
}
