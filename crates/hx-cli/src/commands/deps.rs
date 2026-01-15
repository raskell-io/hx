//! Dependency management commands (add, rm).

use anyhow::Result;
use hx_cabal::{add_dependency, remove_dependency, CabalEditError};
use hx_config::find_project_root;
use hx_ui::Output;
use std::fs;

/// Add a dependency to the project.
pub async fn add(package: &str, _dev: bool, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            output.info("Run `hx init` to create a new project first.");
            return Ok(3);
        }
    };

    // Find the .cabal file
    let cabal_file = find_cabal_file(&project_root);
    let cabal_file = match cabal_file {
        Some(path) => path,
        None => {
            output.error("No .cabal file found in project");
            output.info("Make sure your project has a .cabal file.");
            return Ok(3);
        }
    };

    output.status("Adding", &format!("{} to {}", package, cabal_file.file_name().unwrap().to_string_lossy()));

    match add_dependency(&cabal_file, package, None) {
        Ok(()) => {
            output.status("Added", package);
            Ok(0)
        }
        Err(CabalEditError::DependencyExists(_)) => {
            output.warn(&format!("Dependency '{}' already exists", package));
            Ok(0)
        }
        Err(CabalEditError::NoBuildDepends) => {
            output.error("No build-depends section found in .cabal file");
            output.info("Make sure your .cabal file has a build-depends section.");
            Ok(3)
        }
        Err(e) => {
            output.error(&format!("Failed to add dependency: {}", e));
            Ok(1)
        }
    }
}

/// Remove a dependency from the project.
pub async fn remove(package: &str, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            output.info("Run `hx init` to create a new project first.");
            return Ok(3);
        }
    };

    // Find the .cabal file
    let cabal_file = find_cabal_file(&project_root);
    let cabal_file = match cabal_file {
        Some(path) => path,
        None => {
            output.error("No .cabal file found in project");
            output.info("Make sure your project has a .cabal file.");
            return Ok(3);
        }
    };

    output.status("Removing", &format!("{} from {}", package, cabal_file.file_name().unwrap().to_string_lossy()));

    match remove_dependency(&cabal_file, package) {
        Ok(()) => {
            output.status("Removed", package);
            Ok(0)
        }
        Err(CabalEditError::DependencyNotFound(_)) => {
            output.warn(&format!("Dependency '{}' not found", package));
            Ok(0)
        }
        Err(CabalEditError::NoBuildDepends) => {
            output.error("No build-depends section found in .cabal file");
            Ok(3)
        }
        Err(e) => {
            output.error(&format!("Failed to remove dependency: {}", e));
            Ok(1)
        }
    }
}

/// Find a .cabal file in the given directory.
fn find_cabal_file(dir: &std::path::Path) -> Option<std::path::PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "cabal") {
            return Some(path);
        }
    }
    None
}
