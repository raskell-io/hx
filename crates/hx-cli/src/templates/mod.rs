//! Project templates for `hx new`.

pub mod cli;
pub mod library;
pub mod numeric;
pub mod server;
pub mod webapp;

use hx_config::CompilerBackend;
use hx_solver::bhc_platform::find_platform_for_bhc;
use hx_toolchain::detect_bhc;

/// Template variable substitution context.
pub struct TemplateContext {
    pub project_name: String,
    pub module_name: String,
    pub author: String,
    pub year: String,
    pub backend: Option<CompilerBackend>,
    pub bhc_platform_snapshot: Option<String>,
}

impl TemplateContext {
    pub fn new(project_name: &str) -> Self {
        Self::with_backend(project_name, None)
    }

    pub fn with_backend(project_name: &str, backend: Option<CompilerBackend>) -> Self {
        // Convert project name to module name (kebab-case to PascalCase)
        let module_name = project_name
            .split('-')
            .map(|s| {
                let mut chars = s.chars();
                match chars.next() {
                    None => String::new(),
                    Some(c) => c.to_uppercase().chain(chars).collect(),
                }
            })
            .collect::<String>();

        // Try to get author from git config
        let author = std::process::Command::new("git")
            .args(["config", "user.name"])
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| "Author".to_string());

        let year = chrono::Utc::now().format("%Y").to_string();

        // When using BHC backend, look up matching platform snapshot
        let bhc_platform_snapshot = if backend == Some(CompilerBackend::Bhc) {
            detect_bhc()
                .and_then(|bhc| find_platform_for_bhc(&bhc.version))
                .map(|p| p.id.to_string())
        } else {
            None
        };

        Self {
            project_name: project_name.to_string(),
            module_name,
            author,
            year,
            backend,
            bhc_platform_snapshot,
        }
    }

    /// Substitute template variables in content.
    pub fn substitute(&self, content: &str) -> String {
        let backend_config = match self.backend {
            Some(CompilerBackend::Bhc) => {
                let mut config = "\n[compiler]\nbackend = \"bhc\"\n".to_string();
                if let Some(ref snapshot) = self.bhc_platform_snapshot {
                    config.push_str(&format!("\n[bhc-platform]\nsnapshot = \"{}\"\n", snapshot));
                }
                config
            }
            _ => String::new(),
        };

        content
            .replace("{{project_name}}", &self.project_name)
            .replace("{{module_name}}", &self.module_name)
            .replace("{{author}}", &self.author)
            .replace("{{year}}", &self.year)
            .replace("{{backend_config}}", &backend_config)
    }
}

/// A file to be created as part of a template.
pub struct TemplateFile {
    pub path: &'static str,
    pub content: &'static str,
}
