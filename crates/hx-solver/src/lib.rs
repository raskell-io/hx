//! Dependency solver for Haskell packages.
//!
//! This crate provides:
//! - Version parsing and constraints (PVP-compliant)
//! - Package metadata types
//! - Backtracking dependency resolver
//! - Hackage index parsing
//! - .cabal file parsing
//! - Native package fetching
//! - Build plan generation
//!
//! # Example
//!
//! ```ignore
//! use hx_solver::{load_index, IndexOptions, Resolver, VersionConstraint, default_index_path};
//!
//! // Load the Hackage index
//! let path = default_index_path().expect("Hackage index not found");
//! let index = load_index(&path, &IndexOptions::default())?;
//!
//! // Resolve dependencies
//! let resolver = Resolver::new(&index);
//! let plan = resolver.resolve("aeson", &VersionConstraint::Any)?;
//!
//! for pkg in &plan.packages {
//!     println!("{} {}", pkg.name, pkg.version);
//! }
//! ```

pub mod cache;
pub mod cabal;
pub mod fetch;
pub mod index;
pub mod package;
pub mod plan;
pub mod resolver;
pub mod version;

pub use cache::{
    clear_index_cache, clear_resolution_cache, compute_deps_fingerprint, load_cached_index,
    load_cached_resolution, save_index_cache, save_resolution_cache, CacheError,
};
pub use cabal::{parse_cabal, CabalFile};
pub use fetch::{
    default_package_cache_dir, fetch_packages, FetchError, FetchOptions, FetchResult, FetchSummary,
};
pub use index::{default_index_path, load_index, IndexError, IndexOptions};
pub use package::{
    Dependency, InstallPlan, Package, PackageIndex, PackageVersion, ResolvedPackage,
};
pub use plan::{
    generate_build_plan, BuildPlan, BuildStyle, BuildUnit, PlanError, PlanOptions, PlanSummary,
};
pub use resolver::{ResolveError, Resolver, ResolverConfig};
pub use version::{parse_constraint, Version, VersionConstraint, VersionParseError};
