//! Cabal file editing utilities.
//!
//! Provides simple text-based editing of .cabal files for adding and removing
//! dependencies. Uses a line-based approach to preserve formatting.

use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CabalEditError {
    #[error("failed to read cabal file: {0}")]
    ReadError(#[from] std::io::Error),

    #[error("no build-depends section found in cabal file")]
    NoBuildDepends,

    #[error("dependency '{0}' not found")]
    DependencyNotFound(String),

    #[error("dependency '{0}' already exists")]
    DependencyExists(String),
}

/// Add a dependency to a .cabal file.
///
/// Finds the first `build-depends:` section and adds the package there.
pub fn add_dependency(
    cabal_path: impl AsRef<Path>,
    package: &str,
    version_constraint: Option<&str>,
) -> Result<(), CabalEditError> {
    let content = std::fs::read_to_string(cabal_path.as_ref())?;
    let lines: Vec<&str> = content.lines().collect();

    // Find the build-depends line
    let mut build_depends_idx = None;
    let mut indent = "    ";

    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("build-depends:") {
            build_depends_idx = Some(i);
            // Detect indentation from next line if possible
            if i + 1 < lines.len() {
                let next_line = lines[i + 1];
                let spaces = next_line.len() - next_line.trim_start().len();
                if spaces > 0 && next_line.trim_start().starts_with(',') {
                    indent = &lines[i + 1][..spaces];
                }
            }
            break;
        }
    }

    let build_depends_idx = build_depends_idx.ok_or(CabalEditError::NoBuildDepends)?;

    // Check if dependency already exists
    let package_lower = package.to_lowercase();
    for line in &lines {
        let trimmed = line.trim();
        if trimmed.starts_with(&package_lower)
            || trimmed.starts_with(&format!(",{}", package_lower))
            || trimmed.contains(&format!(" {}", package_lower))
        {
            // Simple check - might have false positives but good enough for common cases
            let dep_part = trimmed.trim_start_matches(',').trim();
            if dep_part.starts_with(&package_lower) {
                return Err(CabalEditError::DependencyExists(package.to_string()));
            }
        }
    }

    // Find where to insert (after the last dependency in this section)
    let mut insert_idx = build_depends_idx + 1;
    for (i, line) in lines.iter().enumerate().skip(build_depends_idx + 1) {
        let trimmed = line.trim();
        // Check if we're still in the build-depends list
        if trimmed.starts_with(',') || trimmed.is_empty() {
            insert_idx = i + 1;
        } else if !trimmed.is_empty() && !line.starts_with(' ') && !line.starts_with('\t') {
            // Hit a new section
            break;
        } else if trimmed.contains(':') && !trimmed.starts_with(',') {
            // Hit a new field
            break;
        } else {
            insert_idx = i + 1;
        }
    }

    // Build the new dependency line
    let dep_line = if let Some(constraint) = version_constraint {
        format!("{}, {} {}", indent, package, constraint)
    } else {
        format!("{}, {}", indent, package)
    };

    // Reconstruct the file
    let mut new_lines: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
    new_lines.insert(insert_idx, dep_line);

    let new_content = new_lines.join("\n");
    std::fs::write(cabal_path.as_ref(), new_content)?;

    Ok(())
}

/// Remove a dependency from a .cabal file.
pub fn remove_dependency(
    cabal_path: impl AsRef<Path>,
    package: &str,
) -> Result<(), CabalEditError> {
    let content = std::fs::read_to_string(cabal_path.as_ref())?;
    let lines: Vec<&str> = content.lines().collect();
    let package_lower = package.to_lowercase();

    let mut found = false;
    let mut new_lines: Vec<String> = Vec::new();

    for line in &lines {
        let trimmed = line.trim();
        let dep_part = trimmed.trim_start_matches(',').trim();

        // Check if this line contains the dependency to remove
        let is_target = dep_part.to_lowercase().starts_with(&package_lower)
            && (dep_part.len() == package_lower.len()
                || dep_part
                    .chars()
                    .nth(package_lower.len())
                    .is_some_and(|c| c == ' ' || c == ','));

        if is_target {
            found = true;
            // Skip this line (don't add to new_lines)
        } else {
            new_lines.push(line.to_string());
        }
    }

    if !found {
        return Err(CabalEditError::DependencyNotFound(package.to_string()));
    }

    let new_content = new_lines.join("\n");
    std::fs::write(cabal_path.as_ref(), new_content)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_add_dependency() {
        let dir = tempdir().unwrap();
        let cabal_path = dir.path().join("test.cabal");

        let initial = r#"cabal-version: 3.0
name: test
version: 0.1.0

executable test
  main-is: Main.hs
  build-depends:
    , base >=4.14 && <5
  default-language: GHC2021
"#;
        std::fs::write(&cabal_path, initial).unwrap();

        add_dependency(&cabal_path, "text", Some(">=2.0")).unwrap();

        let result = std::fs::read_to_string(&cabal_path).unwrap();
        assert!(result.contains("text >=2.0"));
    }

    #[test]
    fn test_remove_dependency() {
        let dir = tempdir().unwrap();
        let cabal_path = dir.path().join("test.cabal");

        let initial = r#"cabal-version: 3.0
name: test
version: 0.1.0

executable test
  main-is: Main.hs
  build-depends:
    , base >=4.14 && <5
    , text >=2.0
  default-language: GHC2021
"#;
        std::fs::write(&cabal_path, initial).unwrap();

        remove_dependency(&cabal_path, "text").unwrap();

        let result = std::fs::read_to_string(&cabal_path).unwrap();
        assert!(!result.contains("text"));
        assert!(result.contains("base"));
    }
}
