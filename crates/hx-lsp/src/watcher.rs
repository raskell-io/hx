//! File system watching with debouncing.
//!
//! This module provides infrastructure for file watching that will be used
//! by the LSP server and watch mode features.

#![allow(dead_code)]

use notify_debouncer_mini::{DebouncedEventKind, Debouncer, new_debouncer};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{Receiver, channel};
use std::time::Duration;
use tracing::{debug, warn};

/// File change event.
#[derive(Debug, Clone)]
pub struct FileChange {
    /// Path to the changed file.
    pub path: PathBuf,
    /// Kind of change.
    pub kind: FileChangeKind,
}

/// Kind of file change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileChangeKind {
    /// File was created or modified.
    Modified,
    /// File was deleted.
    Removed,
}

/// File watcher with debouncing.
pub struct FileWatcher {
    _debouncer: Debouncer<notify::RecommendedWatcher>,
    receiver: Receiver<Vec<FileChange>>,
}

impl FileWatcher {
    /// Create a new file watcher for a directory.
    pub fn new(root: &Path, debounce_ms: u64) -> anyhow::Result<Self> {
        let (tx, rx) = channel();

        let mut debouncer = new_debouncer(
            Duration::from_millis(debounce_ms),
            move |events: Result<Vec<notify_debouncer_mini::DebouncedEvent>, _>| {
                match events {
                    Ok(events) => {
                        let changes: Vec<FileChange> = events
                            .into_iter()
                            .filter_map(|event| {
                                // Only care about Haskell files
                                let path = event.path;
                                if !is_haskell_file(&path) {
                                    return None;
                                }

                                let kind = match event.kind {
                                    DebouncedEventKind::Any => FileChangeKind::Modified,
                                    DebouncedEventKind::AnyContinuous => return None,
                                    _ => return None, // Handle any future variants
                                };

                                Some(FileChange { path, kind })
                            })
                            .collect();

                        if !changes.is_empty()
                            && let Err(e) = tx.send(changes)
                        {
                            warn!("Failed to send file change events: {}", e);
                        }
                    }
                    Err(e) => {
                        warn!("File watch error: {:?}", e);
                    }
                }
            },
        )?;

        // Watch the root directory recursively
        debouncer
            .watcher()
            .watch(root, notify::RecursiveMode::Recursive)?;

        debug!("Started watching {} for changes", root.display());

        Ok(Self {
            _debouncer: debouncer,
            receiver: rx,
        })
    }

    /// Try to receive pending file changes (non-blocking).
    pub fn try_recv(&self) -> Option<Vec<FileChange>> {
        self.receiver.try_recv().ok()
    }

    /// Receive file changes (blocking).
    pub fn recv(&self) -> Option<Vec<FileChange>> {
        self.receiver.recv().ok()
    }
}

/// Check if a path is a Haskell source file.
fn is_haskell_file(path: &Path) -> bool {
    path.extension()
        .map(|ext| ext == "hs" || ext == "lhs")
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_haskell_file() {
        assert!(is_haskell_file(&PathBuf::from("src/Main.hs")));
        assert!(is_haskell_file(&PathBuf::from("src/Lib.lhs")));
        assert!(!is_haskell_file(&PathBuf::from("src/Main.rs")));
        assert!(!is_haskell_file(&PathBuf::from("Cargo.toml")));
        assert!(!is_haskell_file(&PathBuf::from("README.md")));
    }
}
