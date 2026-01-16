//! Package metadata types for dependency resolution.

use crate::version::{Version, VersionConstraint};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A dependency on another package.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    /// Package name
    pub name: String,
    /// Version constraint
    pub constraint: VersionConstraint,
    /// Optional library name (for packages with multiple libraries)
    pub library: Option<String>,
}

impl Dependency {
    /// Create a new dependency with any version.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            constraint: VersionConstraint::Any,
            library: None,
        }
    }

    /// Create a dependency with a version constraint.
    pub fn with_constraint(name: impl Into<String>, constraint: VersionConstraint) -> Self {
        Self {
            name: name.into(),
            constraint,
            library: None,
        }
    }

    /// Check if a version satisfies this dependency.
    pub fn matches(&self, version: &Version) -> bool {
        self.constraint.matches(version)
    }
}

/// Metadata about a single package version.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageVersion {
    /// Package name
    pub name: String,
    /// Version
    pub version: Version,
    /// Build dependencies (required to build)
    pub dependencies: Vec<Dependency>,
    /// Revision number (for .cabal file revisions on Hackage)
    pub revision: u32,
    /// SHA256 hash of the package tarball
    pub hash: Option<String>,
}

impl PackageVersion {
    /// Create a new package version with no dependencies.
    pub fn new(name: impl Into<String>, version: Version) -> Self {
        Self {
            name: name.into(),
            version,
            dependencies: Vec::new(),
            revision: 0,
            hash: None,
        }
    }

    /// Add a dependency.
    pub fn add_dependency(&mut self, dep: Dependency) {
        self.dependencies.push(dep);
    }
}

/// All available versions of a package.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Package {
    /// Package name
    pub name: String,
    /// Available versions (version -> metadata)
    pub versions: HashMap<Version, PackageVersion>,
}

impl Package {
    /// Create a new package with no versions.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            versions: HashMap::new(),
        }
    }

    /// Add a version to this package.
    pub fn add_version(&mut self, pv: PackageVersion) {
        self.versions.insert(pv.version.clone(), pv);
    }

    /// Get all versions sorted in descending order (newest first).
    pub fn versions_descending(&self) -> Vec<&Version> {
        let mut versions: Vec<_> = self.versions.keys().collect();
        versions.sort_by(|a, b| b.cmp(a));
        versions
    }

    /// Get all versions that satisfy a constraint.
    pub fn matching_versions(&self, constraint: &VersionConstraint) -> Vec<&Version> {
        self.versions_descending()
            .into_iter()
            .filter(|v| constraint.matches(v))
            .collect()
    }

    /// Get metadata for a specific version.
    pub fn get_version(&self, version: &Version) -> Option<&PackageVersion> {
        self.versions.get(version)
    }
}

/// The package index containing all available packages.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PackageIndex {
    /// All packages by name
    pub packages: HashMap<String, Package>,
}

impl PackageIndex {
    /// Create an empty package index.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a package version to the index.
    pub fn add_package_version(&mut self, pv: PackageVersion) {
        let package = self
            .packages
            .entry(pv.name.clone())
            .or_insert_with(|| Package::new(&pv.name));
        package.add_version(pv);
    }

    /// Get a package by name.
    pub fn get_package(&self, name: &str) -> Option<&Package> {
        self.packages.get(name)
    }

    /// Get a specific package version.
    pub fn get_package_version(&self, name: &str, version: &Version) -> Option<&PackageVersion> {
        self.get_package(name)?.get_version(version)
    }

    /// Number of packages in the index.
    pub fn package_count(&self) -> usize {
        self.packages.len()
    }

    /// Total number of package versions.
    pub fn version_count(&self) -> usize {
        self.packages.values().map(|p| p.versions.len()).sum()
    }
}

/// A resolved dependency in the installation plan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedPackage {
    /// Package name
    pub name: String,
    /// Resolved version
    pub version: Version,
    /// Direct dependencies (package names)
    pub dependencies: Vec<String>,
}

/// An installation plan with resolved versions.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InstallPlan {
    /// Resolved packages in topological order (dependencies first)
    pub packages: Vec<ResolvedPackage>,
}

impl InstallPlan {
    /// Create an empty installation plan.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a resolved package to the plan.
    pub fn add(&mut self, package: ResolvedPackage) {
        self.packages.push(package);
    }

    /// Get the resolved version for a package.
    pub fn get_version(&self, name: &str) -> Option<&Version> {
        self.packages
            .iter()
            .find(|p| p.name == name)
            .map(|p| &p.version)
    }

    /// Number of packages in the plan.
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Check if the plan is empty.
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_versions_descending() {
        let mut pkg = Package::new("test");
        pkg.add_version(PackageVersion::new("test", "1.0.0".parse().unwrap()));
        pkg.add_version(PackageVersion::new("test", "2.0.0".parse().unwrap()));
        pkg.add_version(PackageVersion::new("test", "1.5.0".parse().unwrap()));

        let versions = pkg.versions_descending();
        assert_eq!(versions.len(), 3);
        assert_eq!(versions[0].to_string(), "2.0.0");
        assert_eq!(versions[1].to_string(), "1.5.0");
        assert_eq!(versions[2].to_string(), "1.0.0");
    }

    #[test]
    fn test_dependency_matches() {
        let dep = Dependency::with_constraint(
            "base",
            VersionConstraint::GreaterThanOrEqual("4.0".parse().unwrap()),
        );

        assert!(dep.matches(&"4.0".parse().unwrap()));
        assert!(dep.matches(&"4.5".parse().unwrap()));
        assert!(!dep.matches(&"3.9".parse().unwrap()));
    }

    #[test]
    fn test_package_index() {
        let mut index = PackageIndex::new();

        let mut pv1 = PackageVersion::new("foo", "1.0.0".parse().unwrap());
        pv1.add_dependency(Dependency::new("base"));
        index.add_package_version(pv1);

        let mut pv2 = PackageVersion::new("foo", "2.0.0".parse().unwrap());
        pv2.add_dependency(Dependency::new("base"));
        index.add_package_version(pv2);

        assert_eq!(index.package_count(), 1);
        assert_eq!(index.version_count(), 2);

        let pkg = index.get_package("foo").unwrap();
        assert_eq!(pkg.versions_descending().len(), 2);
    }
}
