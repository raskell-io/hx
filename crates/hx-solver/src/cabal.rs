//! Simple .cabal file parser for extracting dependencies.
//!
//! This is a minimal parser that extracts build-depends from .cabal files.
//! It doesn't handle the full Cabal specification, just enough for dependency resolution.

use crate::package::Dependency;
use crate::version::{VersionConstraint, parse_constraint};

/// Parsed information from a .cabal file.
#[derive(Debug, Clone, Default)]
pub struct CabalFile {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Build dependencies from the library section
    pub library_deps: Vec<Dependency>,
    /// Build dependencies from executable sections
    pub executable_deps: Vec<Dependency>,
}

impl CabalFile {
    /// Get all unique dependencies.
    pub fn all_dependencies(&self) -> Vec<Dependency> {
        let mut deps = self.library_deps.clone();
        for dep in &self.executable_deps {
            if !deps.iter().any(|d| d.name == dep.name) {
                deps.push(dep.clone());
            }
        }
        deps
    }
}

/// Parse a .cabal file and extract dependencies.
pub fn parse_cabal(content: &str) -> CabalFile {
    let mut result = CabalFile::default();
    let lines = content.lines();
    let mut current_section = Section::TopLevel;
    let mut in_build_depends = false;
    let mut build_depends_buffer = String::new();

    for line in lines {
        let trimmed = line.trim();

        // Skip comments and empty lines
        if trimmed.starts_with("--") || trimmed.is_empty() {
            continue;
        }

        // Check for section headers
        if let Some(section) = parse_section_header(trimmed) {
            // Process any buffered build-depends before switching sections
            if in_build_depends && !build_depends_buffer.is_empty() {
                let deps = parse_build_depends(&build_depends_buffer);
                match current_section {
                    Section::Library => result.library_deps.extend(deps),
                    Section::Executable(_) => result.executable_deps.extend(deps),
                    _ => {}
                }
                build_depends_buffer.clear();
            }
            in_build_depends = false;
            current_section = section;
            continue;
        }

        // Parse top-level fields
        if matches!(current_section, Section::TopLevel) {
            if let Some((key, value)) = parse_field(line) {
                match key.to_lowercase().as_str() {
                    "name" => result.name = value.to_string(),
                    "version" => result.version = value.to_string(),
                    _ => {}
                }
            }
            continue;
        }

        // Parse build-depends in library/executable sections
        if matches!(current_section, Section::Library | Section::Executable(_)) {
            if let Some((key, value)) = parse_field(line)
                && key.to_lowercase() == "build-depends"
            {
                in_build_depends = true;
                build_depends_buffer = value.to_string();
                continue;
            }

            // Continue accumulating build-depends if we're in a continuation
            if in_build_depends {
                if line.starts_with(' ') || line.starts_with('\t') {
                    // Continuation line
                    build_depends_buffer.push_str(trimmed);
                } else {
                    // New field - process the buffer
                    let deps = parse_build_depends(&build_depends_buffer);
                    match current_section {
                        Section::Library => result.library_deps.extend(deps),
                        Section::Executable(_) => result.executable_deps.extend(deps),
                        _ => {}
                    }
                    build_depends_buffer.clear();
                    in_build_depends = false;

                    // Process the new field
                    if let Some((key, value)) = parse_field(line)
                        && key.to_lowercase() == "build-depends"
                    {
                        in_build_depends = true;
                        build_depends_buffer = value.to_string();
                    }
                }
            }
        }
    }

    // Process any remaining build-depends
    if in_build_depends && !build_depends_buffer.is_empty() {
        let deps = parse_build_depends(&build_depends_buffer);
        match current_section {
            Section::Library => result.library_deps.extend(deps),
            Section::Executable(_) => result.executable_deps.extend(deps),
            _ => {}
        }
    }

    result
}

#[derive(Debug, Clone)]
enum Section {
    TopLevel,
    Library,
    #[allow(dead_code)] // Name stored for future use (e.g., filtering deps by executable)
    Executable(String),
    Other,
}

fn parse_section_header(line: &str) -> Option<Section> {
    let lower = line.to_lowercase();

    if lower == "library" {
        return Some(Section::Library);
    }

    if lower.starts_with("executable ") {
        let name = line[11..].trim().to_string();
        return Some(Section::Executable(name));
    }

    if lower.starts_with("test-suite ")
        || lower.starts_with("benchmark ")
        || lower.starts_with("common ")
        || lower.starts_with("source-repository ")
        || lower.starts_with("flag ")
    {
        return Some(Section::Other);
    }

    None
}

fn parse_field(line: &str) -> Option<(&str, &str)> {
    let colon_pos = line.find(':')?;
    let key = line[..colon_pos].trim();
    let value = line[colon_pos + 1..].trim();
    Some((key, value))
}

/// Parse a build-depends field value into dependencies.
fn parse_build_depends(value: &str) -> Vec<Dependency> {
    let mut deps = Vec::new();

    // Split by comma, handling multi-line values
    for part in value.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        if let Some(dep) = parse_single_dependency(part) {
            deps.push(dep);
        }
    }

    deps
}

/// Parse a single dependency like "base >= 4.7 && < 5" or "text"
fn parse_single_dependency(s: &str) -> Option<Dependency> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    // Find where the package name ends and constraint begins
    // Package names can contain hyphens but not spaces or operators
    let constraint_start = s.find(['>', '<', '=', '^']).unwrap_or(s.len());

    let name = s[..constraint_start].trim();
    if name.is_empty() {
        return None;
    }

    // Handle library subcomponent syntax: package:library
    let (package_name, library) = if let Some(colon_pos) = name.find(':') {
        let pkg = &name[..colon_pos];
        let lib = &name[colon_pos + 1..];
        (pkg.trim(), Some(lib.trim().to_string()))
    } else {
        (name, None)
    };

    let constraint_str = s[constraint_start..].trim();
    let constraint = if constraint_str.is_empty() {
        VersionConstraint::Any
    } else {
        parse_constraint(constraint_str).unwrap_or(VersionConstraint::Any)
    };

    Some(Dependency {
        name: package_name.to_string(),
        constraint,
        library,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_cabal() {
        let content = r#"
name:           text
version:        2.1.1
build-type:     Simple

library
  build-depends:
    base >= 4.9 && < 5,
    bytestring >= 0.10.4,
    deepseq
"#;

        let cabal = parse_cabal(content);
        assert_eq!(cabal.name, "text");
        assert_eq!(cabal.version, "2.1.1");
        assert_eq!(cabal.library_deps.len(), 3);

        let base_dep = cabal
            .library_deps
            .iter()
            .find(|d| d.name == "base")
            .unwrap();
        assert!(matches!(base_dep.constraint, VersionConstraint::And(_, _)));
    }

    #[test]
    fn test_parse_dependency_with_constraint() {
        let dep = parse_single_dependency("base >= 4.7 && < 5").unwrap();
        assert_eq!(dep.name, "base");
        assert!(matches!(dep.constraint, VersionConstraint::And(_, _)));
    }

    #[test]
    fn test_parse_dependency_no_constraint() {
        let dep = parse_single_dependency("text").unwrap();
        assert_eq!(dep.name, "text");
        assert!(matches!(dep.constraint, VersionConstraint::Any));
    }

    #[test]
    fn test_parse_dependency_with_library() {
        let dep = parse_single_dependency("containers:containers >= 0.6").unwrap();
        assert_eq!(dep.name, "containers");
        assert_eq!(dep.library, Some("containers".to_string()));
    }

    #[test]
    fn test_parse_caret_constraint() {
        let dep = parse_single_dependency("aeson ^>= 2.2").unwrap();
        assert_eq!(dep.name, "aeson");
        assert!(matches!(dep.constraint, VersionConstraint::Caret(_)));
    }

    #[test]
    fn test_parse_build_depends_multiline() {
        let value = "base >= 4.9, text >= 2.0, bytestring";
        let deps = parse_build_depends(value);
        assert_eq!(deps.len(), 3);
    }
}
