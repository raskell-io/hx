//! Build a single dependency package from source with BHC.
//!
//! Provides utilities for computing deterministic package identifiers and
//! generating GHC-compatible package registration files for packages compiled
//! with BHC.

use crate::native::BhcCompilerConfig;
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

/// Configuration for building a single package.
pub struct BhcPackageBuildConfig {
    /// BHC compiler configuration.
    pub bhc: BhcCompilerConfig,
    /// Directory for intermediate build artifacts.
    pub build_dir: PathBuf,
    /// Directory where the built package will be installed.
    pub install_dir: PathBuf,
    /// Package IDs of already-built dependencies.
    pub dependency_ids: Vec<String>,
    /// Number of parallel compilation jobs.
    pub jobs: usize,
    /// Optimization level (0-3).
    pub optimization: u8,
    /// Enable verbose output.
    pub verbose: bool,
}

/// Result of building a package.
pub struct PackageBuildResult {
    /// The computed deterministic package ID.
    pub package_id: String,
    /// Path to the generated registration (.conf) file.
    pub registration_file: PathBuf,
    /// Path to the installed library archive.
    pub library_path: PathBuf,
}

/// Compute a deterministic package ID.
///
/// The ID is derived from the package name, version, BHC version, profile,
/// and the sorted set of dependency IDs. This ensures that any change in
/// the dependency graph or compiler produces a distinct identifier,
/// enabling content-addressable caching.
///
/// The format is: `<name>-<version>-<hash12>` where `hash12` is the first
/// 12 hex characters of a SHA-256 digest.
pub fn compute_package_id(
    name: &str,
    version: &str,
    bhc_version: &str,
    profile: &str,
    dependency_ids: &[String],
) -> String {
    let mut hasher = Sha256::new();
    hasher.update(name.as_bytes());
    hasher.update(b"-");
    hasher.update(version.as_bytes());
    hasher.update(b"-");
    hasher.update(bhc_version.as_bytes());
    hasher.update(b"-");
    hasher.update(profile.as_bytes());
    let mut sorted_deps = dependency_ids.to_vec();
    sorted_deps.sort();
    for dep in &sorted_deps {
        hasher.update(b"-");
        hasher.update(dep.as_bytes());
    }
    let hash = hex::encode(hasher.finalize());
    format!("{}-{}-{}", name, version, &hash[..12])
}

/// Generate a package registration (.conf) file.
///
/// Produces a GHC-compatible package database entry that can be consumed by
/// `ghc-pkg register` or placed directly in a package database directory.
pub fn generate_registration_file(
    name: &str,
    version: &str,
    package_id: &str,
    install_dir: &Path,
    dependency_ids: &[String],
    exposed_modules: &[String],
) -> String {
    let lib_dir = install_dir.join("lib");
    let depends_str = dependency_ids.join(" ");
    let modules_str = exposed_modules.join(" ");

    format!(
        "name: {name}\n\
         version: {version}\n\
         id: {package_id}\n\
         key: {package_id}\n\
         library-dirs: {lib_dir}\n\
         hs-libraries: HS{package_id}\n\
         import-dirs: {lib_dir}\n\
         depends: {depends_str}\n\
         exposed-modules: {modules_str}\n",
        name = name,
        version = version,
        package_id = package_id,
        lib_dir = lib_dir.display(),
        depends_str = depends_str,
        modules_str = modules_str,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_package_id() {
        let id1 = compute_package_id("text", "2.1.0", "2026.2.0", "default", &[]);
        let id2 = compute_package_id("text", "2.1.0", "2026.2.0", "default", &[]);

        // Deterministic: same inputs produce the same output
        assert_eq!(id1, id2);

        // Format check
        assert!(id1.starts_with("text-2.1.0-"));
        // Hash portion is 12 hex chars
        let hash_part = id1.strip_prefix("text-2.1.0-").unwrap();
        assert_eq!(hash_part.len(), 12);
        assert!(hash_part.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn test_compute_package_id_changes_with_deps() {
        let id_no_deps = compute_package_id("text", "2.1.0", "2026.2.0", "default", &[]);
        let id_with_deps = compute_package_id(
            "text",
            "2.1.0",
            "2026.2.0",
            "default",
            &["base-4.18.0-abc123".to_string()],
        );

        assert_ne!(id_no_deps, id_with_deps);
    }

    #[test]
    fn test_compute_package_id_changes_with_profile() {
        let id_default = compute_package_id("text", "2.1.0", "2026.2.0", "default", &[]);
        let id_numeric = compute_package_id("text", "2.1.0", "2026.2.0", "numeric", &[]);

        assert_ne!(id_default, id_numeric);
    }

    #[test]
    fn test_compute_package_id_dep_order_invariant() {
        let deps_a = vec!["base-1.0-aaa".to_string(), "text-2.0-bbb".to_string()];
        let deps_b = vec!["text-2.0-bbb".to_string(), "base-1.0-aaa".to_string()];

        let id_a = compute_package_id("aeson", "2.2.0", "2026.2.0", "default", &deps_a);
        let id_b = compute_package_id("aeson", "2.2.0", "2026.2.0", "default", &deps_b);

        // Sorted internally, so order should not matter
        assert_eq!(id_a, id_b);
    }

    #[test]
    fn test_generate_registration_file() {
        let content = generate_registration_file(
            "text",
            "2.1.0",
            "text-2.1.0-abc123def456",
            Path::new("/home/user/.cache/hx/store/text-2.1.0-abc123def456"),
            &[
                "base-4.18.0-xyz789".to_string(),
                "bytestring-0.12.0-def456".to_string(),
            ],
            &[
                "Data.Text".to_string(),
                "Data.Text.IO".to_string(),
                "Data.Text.Lazy".to_string(),
            ],
        );

        assert!(content.contains("name: text"));
        assert!(content.contains("version: 2.1.0"));
        assert!(content.contains("id: text-2.1.0-abc123def456"));
        assert!(content.contains("key: text-2.1.0-abc123def456"));
        assert!(content.contains("hs-libraries: HStext-2.1.0-abc123def456"));
        assert!(content.contains("depends: base-4.18.0-xyz789 bytestring-0.12.0-def456"));
        assert!(content.contains("exposed-modules: Data.Text Data.Text.IO Data.Text.Lazy"));

        // Library and import dirs should point to install_dir/lib
        assert!(content.contains("library-dirs:"));
        assert!(content.contains("import-dirs:"));
        let lib_path = "/home/user/.cache/hx/store/text-2.1.0-abc123def456/lib";
        assert!(content.contains(lib_path));
    }

    #[test]
    fn test_generate_registration_file_no_deps() {
        let content = generate_registration_file(
            "base",
            "4.18.0",
            "base-4.18.0-aaa111bbb222",
            Path::new("/opt/bhc/lib"),
            &[],
            &["Prelude".to_string(), "Data.List".to_string()],
        );

        assert!(content.contains("name: base"));
        assert!(content.contains("depends: \n") || content.contains("depends: "));
        assert!(content.contains("exposed-modules: Prelude Data.List"));
    }
}
