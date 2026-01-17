# hx.toml Manifest Reference

The `hx.toml` file is the project manifest for hx projects.

## Complete Example

```toml
[project]
name = "my-app"
kind = "bin"
resolver = "cabal"

[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
hls = "2.9.0.0"

[build]
optimization = 1
warnings = true
werror = false
ghc_flags = ["-Wall", "-Wcompat"]
src_dirs = ["src", "app"]
native = false

[format]
formatter = "fourmolu"

[lint]
hlint = true

[plugins]
enabled = true
hook_timeout_ms = 5000
paths = ["plugins"]
continue_on_error = false

[plugins.hooks]
pre_build = ["./scripts/pre-build.sh"]
post_build = []
pre_test = []
post_test = ["./scripts/notify.sh"]
pre_run = []
post_run = []
pre_clean = []
post_clean = []
pre_lock = []
post_lock = []
init = []

[dependencies]
base = ">=4.18 && <5"
text = "^2.0"
aeson = ">=2.0"
```

## Sections

### [project]

Project metadata.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | string | yes | - | Project name |
| `kind` | `"bin"` \| `"lib"` | no | `"bin"` | Project type |
| `resolver` | string | no | `"cabal"` | Build system |

```toml
[project]
name = "hello-world"
kind = "bin"
resolver = "cabal"
```

### [toolchain]

Toolchain version pinning.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `ghc` | string | no | - | GHC version |
| `cabal` | string | no | - | Cabal version |
| `hls` | string | no | - | HLS version |

```toml
[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
```

When versions are specified, `hx build` will auto-install them if missing.

### [build]

Build configuration.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `optimization` | 0-2 | no | `1` | Optimization level |
| `warnings` | bool | no | `true` | Enable warnings |
| `werror` | bool | no | `false` | Treat warnings as errors |
| `ghc_flags` | string[] | no | `[]` | Extra GHC flags |
| `src_dirs` | string[] | no | `["src"]` | Source directories |
| `native` | bool | no | `false` | Use native builder |

```toml
[build]
optimization = 2
warnings = true
werror = true
ghc_flags = ["-Wall", "-Wcompat", "-Widentities"]
src_dirs = ["src", "app"]
```

### [format]

Code formatting settings.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `formatter` | `"fourmolu"` \| `"ormolu"` | no | `"fourmolu"` | Formatter to use |

```toml
[format]
formatter = "fourmolu"
```

### [lint]

Linting configuration.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `hlint` | bool | no | `false` | Enable HLint |

```toml
[lint]
hlint = true
```

### [plugins]

Plugin system configuration.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `enabled` | bool | no | `true` | Enable plugins |
| `hook_timeout_ms` | u64 | no | `5000` | Hook timeout |
| `paths` | string[] | no | `[]` | Plugin search paths |
| `continue_on_error` | bool | no | `false` | Continue on hook failure |

### [plugins.hooks]

Hook scripts for lifecycle events.

| Hook | When |
|------|------|
| `pre_build` | Before build starts |
| `post_build` | After build completes |
| `pre_test` | Before tests run |
| `post_test` | After tests complete |
| `pre_run` | Before run command |
| `post_run` | After run completes |
| `pre_clean` | Before clean |
| `post_clean` | After clean |
| `pre_lock` | Before lock generation |
| `post_lock` | After lock generation |
| `init` | After project initialization |

```toml
[plugins.hooks]
pre_build = ["cargo fmt --check"]
post_test = ["./scripts/coverage-report.sh"]
```

### [dependencies]

Direct dependencies with version constraints.

```toml
[dependencies]
base = ">=4.18 && <5"
text = "^2.0"
aeson = ">=2.0"
bytestring = "*"
```

#### Version Constraint Syntax

| Syntax | Meaning |
|--------|---------|
| `"*"` | Any version |
| `"1.2.3"` | Exactly 1.2.3 |
| `">=1.2"` | 1.2 or higher |
| `"<2.0"` | Below 2.0 |
| `">=1.0 && <2.0"` | Range |
| `"^1.2"` | Compatible with 1.2 (PVP) |

## Manifest Types

```rust
pub struct Manifest {
    pub project: ProjectConfig,
    pub toolchain: ToolchainConfig,
    pub build: BuildConfig,
    pub format: FormatConfig,
    pub lint: LintConfig,
    pub plugins: PluginsConfig,
    pub dependencies: HashMap<String, String>,
}

pub struct ProjectConfig {
    pub name: String,
    pub kind: ProjectKind,
    pub resolver: String,
}

pub enum ProjectKind {
    Bin,
    Lib,
}

pub struct ToolchainConfig {
    pub ghc: Option<String>,
    pub cabal: Option<String>,
    pub hls: Option<String>,
}
```

## Loading Manifests

```rust
use hx_config::Manifest;

// From file
let manifest = Manifest::from_file("hx.toml")?;

// From string
let manifest = Manifest::parse(toml_content)?;

// Access fields
println!("Name: {}", manifest.project.name);
println!("GHC: {:?}", manifest.toolchain.ghc);
```

## Validation

Manifests are validated on load:

- Project name must be valid
- Version strings must be valid semver
- Referenced paths must exist (optional)
- Dependencies must have valid constraints
