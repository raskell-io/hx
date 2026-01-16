//! Dependency solver for Haskell packages.
//!
//! This crate provides:
//! - Version parsing and constraints (PVP-compliant)
//! - Package metadata types
//! - Backtracking dependency resolver
//! - Hackage index parsing (planned)
//!
//! # Example
//!
//! ```ignore
//! use hx_solver::{PackageIndex, Resolver, VersionConstraint};
//!
//! let index = PackageIndex::load_from_hackage()?;
//! let resolver = Resolver::new(&index);
//!
//! let plan = resolver.resolve("aeson", &VersionConstraint::Any)?;
//! for pkg in &plan.packages {
//!     println!("{} {}", pkg.name, pkg.version);
//! }
//! ```

pub mod package;
pub mod resolver;
pub mod version;

pub use package::{
    Dependency, InstallPlan, Package, PackageIndex, PackageVersion, ResolvedPackage,
};
pub use resolver::{ResolveError, Resolver, ResolverConfig};
pub use version::{Version, VersionConstraint, VersionParseError, parse_constraint};
