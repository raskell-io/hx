# Project Detection

`hx-config` provides utilities for discovering and loading hx projects.

## Finding Project Root

The `find_project_root` function searches upward for an `hx.toml` file:

```rust
use hx_config::find_project_root;

// From current directory
let root = find_project_root(".")?;

// From a specific path
let root = find_project_root("/path/to/subdir")?;
```

### Search Algorithm

1. Start from the given path
2. Check if `hx.toml` exists in current directory
3. If not found, move to parent directory
4. Repeat until root (`/`) is reached
5. Return error if not found

### Error Handling

```rust
match find_project_root(".") {
    Ok(root) => println!("Found: {}", root.display()),
    Err(e) => {
        // Error includes searched paths
        eprintln!("{}", e);
        // Output: "no hx project found (searched: /path/to/cwd, /path/to, /path, /)"
    }
}
```

## Loading Projects

The `Project` struct represents a loaded project:

```rust
use hx_config::Project;

let project = Project::load(&root)?;
```

### Project Structure

```rust
pub struct Project {
    /// Project root directory
    pub root: PathBuf,

    /// Parsed manifest (hx.toml)
    pub manifest: Manifest,

    /// Path to .cabal file (if found)
    pub cabal_file: Option<PathBuf>,

    /// Whether cabal.project exists
    pub has_cabal_project: bool,

    /// Workspace packages (if multi-package)
    pub workspace_packages: Vec<WorkspacePackage>,
}
```

### Project Methods

```rust
impl Project {
    /// Get project name
    pub fn name(&self) -> &str;

    /// Get hx.toml path
    pub fn manifest_path(&self) -> PathBuf;

    /// Get hx.lock path
    pub fn lockfile_path(&self) -> PathBuf;

    /// Get .hx cache directory
    pub fn cache_dir(&self) -> PathBuf;

    /// Get cabal build output directory
    pub fn cabal_build_dir(&self) -> PathBuf;

    /// Check if workspace (multiple packages)
    pub fn is_workspace(&self) -> bool;

    /// Get workspace package count
    pub fn package_count(&self) -> usize;

    /// Get list of package names
    pub fn package_names(&self) -> Vec<&str>;

    /// Get package by name
    pub fn get_package(&self, name: &str) -> Option<&WorkspacePackage>;
}
```

## Project Layout

hx expects this directory structure:

```
my-project/
├── hx.toml              # Project manifest
├── hx.lock              # Lockfile (generated)
├── .hx/                 # Cache directory
│   ├── build/           # Build artifacts
│   └── fingerprint      # Source fingerprint
├── cabal.project        # Cabal project file (optional)
├── my-project.cabal     # Package description
└── src/
    └── Main.hs
```

## Cabal File Discovery

Projects can have associated `.cabal` files:

```rust
if let Some(cabal_path) = &project.cabal_file {
    println!("Cabal file: {}", cabal_path.display());
}
```

### Discovery Rules

1. Look for `{project-name}.cabal` in root
2. Look for any `.cabal` file in root
3. For workspaces, each package has its own `.cabal` file

## Constants

```rust
/// Manifest filename
pub const MANIFEST_FILENAME: &str = "hx.toml";

/// Lockfile filename
pub const LOCKFILE_FILENAME: &str = "hx.lock";

/// Cache directory name
pub const CACHE_DIR_NAME: &str = ".hx";
```

## Example: Full Project Loading

```rust
use hx_config::{find_project_root, Project};

fn main() -> anyhow::Result<()> {
    // Find and load project
    let root = find_project_root(".")?;
    let project = Project::load(&root)?;

    // Print project info
    println!("Project: {}", project.name());
    println!("Root: {}", project.root.display());
    println!("Kind: {:?}", project.manifest.project.kind);

    // Toolchain requirements
    if let Some(ghc) = &project.manifest.toolchain.ghc {
        println!("Required GHC: {}", ghc);
    }

    // Workspace info
    if project.is_workspace() {
        println!("Packages ({}):", project.package_count());
        for name in project.package_names() {
            println!("  - {}", name);
        }
    }

    Ok(())
}
```
