//! Filesystem API for plugins.
//!
//! Provides: (hx/read-file), (hx/write-file), (hx/file-exists?), (hx/glob), (hx/path-join)

use crate::context::with_context;
use crate::error::Result;
use std::fs;
use std::path::PathBuf;
use steel::SteelVal;
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;

/// Register filesystem API functions.
pub fn register(engine: &mut Engine) -> Result<()> {
    engine.register_fn("hx/read-file", read_file);
    engine.register_fn("hx/write-file", write_file);
    engine.register_fn("hx/file-exists?", file_exists);
    engine.register_fn("hx/glob", glob_files);
    engine.register_fn("hx/path-join", path_join);
    engine.register_fn("hx/mkdir", mkdir);
    Ok(())
}

/// Read a file's contents.
fn read_file(path: String) -> std::result::Result<SteelVal, String> {
    let resolved_path = resolve_path(&path);

    fs::read_to_string(&resolved_path)
        .map(|content| SteelVal::StringV(content.into()))
        .map_err(|e| format!("Failed to read file '{}': {}", path, e))
}

/// Write content to a file.
fn write_file(path: String, content: String) -> std::result::Result<SteelVal, String> {
    let resolved_path = resolve_path(&path);

    // Create parent directories if needed
    if let Some(parent) = resolved_path.parent() {
        fs::create_dir_all(parent).map_err(|e| format!("Failed to create directory: {}", e))?;
    }

    fs::write(&resolved_path, content)
        .map(|_| SteelVal::Void)
        .map_err(|e| format!("Failed to write file '{}': {}", path, e))
}

/// Check if a file exists.
fn file_exists(path: String) -> SteelVal {
    let resolved_path = resolve_path(&path);
    SteelVal::BoolV(resolved_path.exists())
}

/// Find files matching a glob pattern.
fn glob_files(pattern: String) -> SteelVal {
    let resolved_pattern = resolve_path(&pattern);
    let pattern_str = resolved_pattern.to_string_lossy();

    match glob::glob(&pattern_str) {
        Ok(paths) => {
            let files: Vec<SteelVal> = paths
                .filter_map(|p| p.ok())
                .map(|p| SteelVal::StringV(p.to_string_lossy().to_string().into()))
                .collect();
            SteelVal::ListV(files.into())
        }
        Err(e) => {
            eprintln!("Glob error: {}", e);
            SteelVal::ListV(vec![].into())
        }
    }
}

/// Join path components.
fn path_join(parts: Vec<SteelVal>) -> SteelVal {
    let mut path = PathBuf::new();

    for part in parts {
        if let SteelVal::StringV(s) = part {
            path.push(s.to_string());
        }
    }

    SteelVal::StringV(path.to_string_lossy().to_string().into())
}

/// Create a directory (and parents).
fn mkdir(path: String) -> std::result::Result<SteelVal, String> {
    let resolved_path = resolve_path(&path);

    fs::create_dir_all(&resolved_path)
        .map(|_| SteelVal::Void)
        .map_err(|e| format!("Failed to create directory '{}': {}", path, e))
}

/// Resolve a path relative to the project root.
fn resolve_path(path: &str) -> PathBuf {
    let path = PathBuf::from(path);

    // If absolute, return as-is
    if path.is_absolute() {
        return path;
    }

    // Otherwise, resolve relative to project root
    with_context(|ctx| ctx.project_root.join(&path)).unwrap_or(path)
}
