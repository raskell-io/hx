//! Configuration merging utilities.
//!
//! This module provides the `Combine` trait for merging configuration from
//! multiple sources, following Cargo's config.toml merging rules:
//!
//! - Numbers, strings, booleans: higher precedence config takes precedence
//! - Arrays: merged with higher precedence items placed earlier
//! - `Option<T>`: first `Some` value wins

use crate::{FormatConfig, LintConfig, Manifest, ProjectConfig, ToolchainConfig};

/// Trait for combining configuration values.
///
/// The convention is that `self` has higher precedence than `other`.
pub trait Combine {
    /// Combine two values, preferring values in `self`.
    #[must_use]
    fn combine(self, other: Self) -> Self;
}

// ─── Option<T> ───────────────────────────────────────────────────────────────

impl<T> Combine for Option<T> {
    fn combine(self, other: Self) -> Self {
        self.or(other)
    }
}

// ─── Vec<T> ──────────────────────────────────────────────────────────────────

impl<T> Combine for Vec<T> {
    fn combine(mut self, other: Self) -> Self {
        self.extend(other);
        self
    }
}

// ─── Manifest ────────────────────────────────────────────────────────────────

impl Combine for Manifest {
    fn combine(self, other: Self) -> Self {
        Self {
            project: self.project.combine(other.project),
            toolchain: self.toolchain.combine(other.toolchain),
            format: self.format.combine(other.format),
            lint: self.lint.combine(other.lint),
            dependencies: {
                let mut deps = self.dependencies;
                for (k, v) in other.dependencies {
                    deps.entry(k).or_insert(v);
                }
                deps
            },
        }
    }
}

impl Combine for ProjectConfig {
    fn combine(self, other: Self) -> Self {
        Self {
            // Project name should come from the higher-precedence source
            name: if self.name.is_empty() {
                other.name
            } else {
                self.name
            },
            kind: self.kind, // Use self's kind
            resolver: self.resolver,
        }
    }
}

impl Combine for ToolchainConfig {
    fn combine(self, other: Self) -> Self {
        Self {
            ghc: self.ghc.combine(other.ghc),
            cabal: self.cabal.combine(other.cabal),
            hls: self.hls.combine(other.hls),
        }
    }
}

impl Combine for FormatConfig {
    fn combine(self, _other: Self) -> Self {
        Self {
            formatter: self.formatter, // Prefer self
        }
    }
}

impl Combine for LintConfig {
    fn combine(self, _other: Self) -> Self {
        Self {
            hlint: self.hlint, // Prefer self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_option_combine() {
        assert_eq!(Some(1).combine(Some(2)), Some(1));
        assert_eq!(None::<i32>.combine(Some(2)), Some(2));
        assert_eq!(Some(1).combine(None), Some(1));
        assert_eq!(None::<i32>.combine(None), None);
    }

    #[test]
    fn test_vec_combine() {
        let a = vec![1, 2];
        let b = vec![3, 4];
        assert_eq!(a.combine(b), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_toolchain_combine() {
        let a = ToolchainConfig {
            ghc: Some("9.8.2".to_string()),
            cabal: None,
            hls: None,
        };
        let b = ToolchainConfig {
            ghc: Some("9.6.4".to_string()),
            cabal: Some("3.12.1.0".to_string()),
            hls: Some("2.9.0.0".to_string()),
        };
        let combined = a.combine(b);
        assert_eq!(combined.ghc, Some("9.8.2".to_string())); // Self wins
        assert_eq!(combined.cabal, Some("3.12.1.0".to_string())); // From other
        assert_eq!(combined.hls, Some("2.9.0.0".to_string())); // From other
    }
}
