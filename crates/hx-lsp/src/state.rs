//! Server and workspace state management.

use dashmap::DashMap;
use hx_core::{DiagnosticReport, GhcDiagnostic};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::RwLock;

/// State for a single workspace/project.
#[derive(Debug)]
pub struct WorkspaceState {
    /// Root path of the workspace.
    pub root: PathBuf,
    /// GHC version detected for this workspace.
    pub ghc_version: Option<String>,
    /// Whether JSON diagnostics are supported.
    pub supports_json: bool,
    /// Current diagnostics by file.
    pub diagnostics: DashMap<PathBuf, Vec<GhcDiagnostic>>,
}

impl WorkspaceState {
    /// Create a new workspace state.
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            ghc_version: None,
            supports_json: false,
            diagnostics: DashMap::new(),
        }
    }

    /// Update diagnostics from a report.
    pub fn update_diagnostics(&self, report: DiagnosticReport) {
        // Clear old diagnostics for files in the report
        for file in report.files() {
            self.diagnostics.remove(&file.clone());
        }

        // Insert new diagnostics
        for (file, diags) in report.by_file {
            self.diagnostics.insert(file, diags);
        }
    }

    /// Get diagnostics for a specific file.
    pub fn get_diagnostics(&self, file: &PathBuf) -> Vec<GhcDiagnostic> {
        self.diagnostics
            .get(file)
            .map(|r| r.value().clone())
            .unwrap_or_default()
    }

    /// Clear all diagnostics.
    pub fn clear_diagnostics(&self) {
        self.diagnostics.clear();
    }
}

/// Global server state.
#[derive(Debug, Default)]
pub struct ServerState {
    /// Active workspaces by root path.
    workspaces: DashMap<PathBuf, Arc<WorkspaceState>>,
    /// Initialization status.
    initialized: RwLock<bool>,
}

impl ServerState {
    /// Create a new server state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a workspace.
    pub fn add_workspace(&self, root: PathBuf) -> Arc<WorkspaceState> {
        let state = Arc::new(WorkspaceState::new(root.clone()));
        self.workspaces.insert(root, state.clone());
        state
    }

    /// Get a workspace by root path.
    pub fn get_workspace(&self, root: &PathBuf) -> Option<Arc<WorkspaceState>> {
        self.workspaces.get(root).map(|r| r.value().clone())
    }

    /// Find workspace containing a file.
    pub fn find_workspace(&self, file: &Path) -> Option<Arc<WorkspaceState>> {
        for entry in self.workspaces.iter() {
            if file.starts_with(entry.key()) {
                return Some(entry.value().clone());
            }
        }
        None
    }

    /// Mark the server as initialized.
    pub async fn set_initialized(&self, value: bool) {
        let mut init = self.initialized.write().await;
        *init = value;
    }

    /// Check if the server is initialized.
    pub async fn is_initialized(&self) -> bool {
        *self.initialized.read().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hx_core::{DiagnosticSeverity, SourceSpan};

    #[test]
    fn test_workspace_state_new() {
        let state = WorkspaceState::new(PathBuf::from("/project"));
        assert_eq!(state.root, PathBuf::from("/project"));
        assert!(state.ghc_version.is_none());
        assert!(!state.supports_json);
    }

    #[test]
    fn test_workspace_diagnostics() {
        let state = WorkspaceState::new(PathBuf::from("/project"));

        let mut report = DiagnosticReport::new();
        report.add(GhcDiagnostic {
            span: Some(SourceSpan::new(
                PathBuf::from("/project/src/Main.hs"),
                10,
                1,
                10,
                5,
            )),
            severity: DiagnosticSeverity::Error,
            code: None,
            warning_flag: None,
            message: "Test error".to_string(),
            hints: vec![],
            fixes: vec![],
        });

        state.update_diagnostics(report);

        let diags = state.get_diagnostics(&PathBuf::from("/project/src/Main.hs"));
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Test error");
    }

    #[test]
    fn test_server_state_workspaces() {
        let state = ServerState::new();

        let _ws1 = state.add_workspace(PathBuf::from("/project1"));
        let _ws2 = state.add_workspace(PathBuf::from("/project2"));

        assert!(state.get_workspace(&PathBuf::from("/project1")).is_some());
        assert!(state.get_workspace(&PathBuf::from("/project2")).is_some());
        assert!(state.get_workspace(&PathBuf::from("/project3")).is_none());

        // Find workspace by file
        let found = state.find_workspace(&PathBuf::from("/project1/src/Main.hs"));
        assert!(found.is_some());
        assert_eq!(found.unwrap().root, PathBuf::from("/project1"));
    }
}
