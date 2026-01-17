# Lockfile Format

The `hx.lock` file uses TOML format for human readability and easy diffing.

## Complete Example

```toml
version = 1
created_at = "2024-01-15T10:30:00Z"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"

[plan]
compiler_id = "ghc-9.8.2"
platform = "aarch64-apple-darwin"
index_state = "2024-01-15T00:00:00Z"
hash = "sha256:abc123def456..."

[workspace]
is_workspace = true

[[workspace.packages]]
name = "my-lib"
version = "0.1.0"
path = "packages/lib"

[[workspace.packages]]
name = "my-app"
version = "1.0.0"
path = "packages/app"

[[packages]]
name = "aeson"
version = "2.2.1.0"
source = "hackage"
hash = "sha256:9f5..."

[[packages]]
name = "base"
version = "4.19.0.0"
source = "ghc"

[[packages]]
name = "text"
version = "2.1"
source = "hackage"
hash = "sha256:abc..."
```

## Sections

### Root Fields

| Field | Type | Description |
|-------|------|-------------|
| `version` | u32 | Lockfile format version (currently 1) |
| `created_at` | ISO 8601 | Timestamp when lockfile was generated |

### [toolchain]

Records toolchain versions used for resolution.

| Field | Type | Description |
|-------|------|-------------|
| `ghc` | string? | GHC version |
| `cabal` | string? | Cabal version |

### [plan]

Build plan metadata.

| Field | Type | Description |
|-------|------|-------------|
| `compiler_id` | string? | Full compiler identifier (e.g., "ghc-9.8.2") |
| `platform` | string? | Target platform |
| `index_state` | string? | Hackage index timestamp |
| `hash` | string? | Plan content hash |

### [workspace]

Workspace information.

| Field | Type | Description |
|-------|------|-------------|
| `is_workspace` | bool | Whether this is a multi-package project |

### [[workspace.packages]]

Array of workspace packages.

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Package name |
| `version` | string | Package version |
| `path` | string | Relative path from workspace root |

### [[packages]]

Array of locked dependency versions.

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Package name |
| `version` | string | Exact version |
| `source` | string | Package source ("hackage", "ghc", "git") |
| `hash` | string? | Content hash for verification |

## Lockfile Types

```rust
pub const LOCK_VERSION: u32 = 1;

pub struct Lockfile {
    pub version: u32,
    pub created_at: DateTime<Utc>,
    pub toolchain: LockedToolchain,
    pub plan: LockedPlan,
    pub workspace: LockedWorkspace,
    pub packages: Vec<LockedPackage>,
}

pub struct LockedToolchain {
    pub ghc: Option<String>,
    pub cabal: Option<String>,
}

pub struct LockedPlan {
    pub compiler_id: Option<String>,
    pub platform: Option<String>,
    pub index_state: Option<String>,
    pub hash: Option<String>,
}

pub struct LockedWorkspace {
    pub is_workspace: bool,
    pub packages: Vec<WorkspacePackageInfo>,
}

pub struct WorkspacePackageInfo {
    pub name: String,
    pub version: String,
    pub path: String,
}

pub struct LockedPackage {
    pub name: String,
    pub version: String,
    pub source: String,
    pub hash: Option<String>,
}
```

## Loading and Saving

```rust
use hx_lock::Lockfile;

// Load from file
let lockfile = Lockfile::from_file("hx.lock")?;

// Parse from string
let lockfile = Lockfile::parse(toml_content)?;

// Serialize to string
let content = lockfile.to_string()?;

// Save to file
lockfile.to_file("hx.lock")?;
```

## Version Compatibility

The lockfile version field enables forward compatibility:

```rust
match Lockfile::from_file("hx.lock") {
    Ok(lock) => { /* use lockfile */ }
    Err(LockError::VersionMismatch { expected, found }) => {
        eprintln!("Lockfile version {} not supported (expected {})", found, expected);
    }
    Err(e) => { /* other error */ }
}
```

## Lockfile Operations

### Creating a New Lockfile

```rust
let mut lockfile = Lockfile::new();
lockfile.set_toolchain(Some("9.8.2".into()), Some("3.12.1.0".into()));
lockfile.add_package(LockedPackage {
    name: "aeson".into(),
    version: "2.2.1.0".into(),
    source: "hackage".into(),
    hash: Some("sha256:...".into()),
});
lockfile.to_file("hx.lock")?;
```

### Updating Packages

```rust
let mut lockfile = Lockfile::from_file("hx.lock")?;

// Add or update package
lockfile.add_package(LockedPackage {
    name: "text".into(),
    version: "2.1".into(),
    source: "hackage".into(),
    hash: None,
});

lockfile.to_file("hx.lock")?;
```

### Checking Workspace Status

```rust
let lockfile = Lockfile::from_file("hx.lock")?;

if lockfile.is_workspace() {
    for name in lockfile.workspace_package_names() {
        println!("Workspace package: {}", name);
    }
}
```

## Determinism

Lockfiles are designed for deterministic serialization:

1. **Sorted packages** - Packages ordered alphabetically by name
2. **Consistent timestamps** - UTC timezone always
3. **Normalized paths** - Forward slashes on all platforms
4. **Stable hashes** - Same content = same hash
