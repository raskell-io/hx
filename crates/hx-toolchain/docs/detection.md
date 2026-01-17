# Tool Detection

`hx-toolchain` provides comprehensive tool detection.

## Toolchain Struct

```rust
pub struct Toolchain {
    pub ghc: DetectedTool,
    pub cabal: DetectedTool,
    pub ghcup: DetectedTool,
    pub hls: DetectedTool,
}
```

## Detecting Tools

```rust
use hx_toolchain::Toolchain;

let toolchain = Toolchain::detect().await;

// Check each tool
if toolchain.ghc.status.is_found() {
    let version = toolchain.ghc.status.version().unwrap();
    let path = toolchain.ghc.status.path().unwrap();
    println!("GHC {} at {}", version, path.display());
}
```

## DetectedTool

```rust
pub struct DetectedTool {
    /// Tool status (found/not found, version, path)
    pub status: ToolStatus,
    /// Explanation of how tool was found/why not found
    pub why: Option<String>,
}
```

## ToolStatus

```rust
pub enum ToolStatus {
    /// Tool found with version and path
    Found {
        version: Option<Version>,
        path: PathBuf,
    },
    /// Tool not found
    NotFound,
}

impl ToolStatus {
    /// Check if tool was found
    pub fn is_found(&self) -> bool;

    /// Get version if found
    pub fn version(&self) -> Option<&Version>;

    /// Get path if found
    pub fn path(&self) -> Option<&PathBuf>;
}
```

## Detection Order

Tools are searched in this order:

### GHC
1. hx-managed: `~/.hx/toolchains/ghc/{version}/bin/ghc`
2. ghcup-managed: `~/.ghcup/bin/ghc`
3. System PATH: `which ghc`

### Cabal
1. hx-managed: `~/.hx/toolchains/cabal/{version}/bin/cabal`
2. ghcup-managed: `~/.ghcup/bin/cabal`
3. System PATH: `which cabal`

### GHCup
1. Standard location: `~/.ghcup/bin/ghcup`
2. System PATH: `which ghcup`

### HLS
1. ghcup-managed: `~/.ghcup/bin/haskell-language-server-wrapper`
2. System PATH: `which haskell-language-server-wrapper`

## Version Parsing

Version strings are parsed from tool output:

```rust
// GHC
// Input: "The Glorious Glasgow Haskell Compilation System, version 9.8.2"
// Output: Version { major: 9, minor: 8, patch: 2 }

// Cabal
// Input: "cabal-install version 3.12.1.0"
// Output: Version { major: 3, minor: 12, patch: 1, extra: 0 }
```

## Detection Examples

```rust
use hx_toolchain::Toolchain;

let toolchain = Toolchain::detect().await;

// Print detection report
println!("GHC: {}", match &toolchain.ghc.status {
    ToolStatus::Found { version, path } => {
        format!("{} ({})", version.as_ref().unwrap(), path.display())
    }
    ToolStatus::NotFound => "not found".into(),
});

// Access explanation
if let Some(why) = &toolchain.ghc.why {
    println!("  {}", why);
}
```

## Checking Requirements

```rust
use hx_toolchain::{Toolchain, check_requirements};

let toolchain = Toolchain::detect().await;

// Check if toolchain meets requirements
let check = check_requirements(
    &toolchain,
    Some("9.8.2"),   // Required GHC
    Some("3.12.1.0"), // Required Cabal
);

if !check.ghc_ok {
    println!("GHC mismatch: need {}, have {:?}",
        check.required_ghc.unwrap(),
        toolchain.ghc.status.version()
    );
}
```

## Platform Differences

### Linux
- GHCup default: `~/.ghcup/`
- hx-managed: `~/.hx/toolchains/`

### macOS
- GHCup default: `~/.ghcup/`
- hx-managed: `~/.hx/toolchains/`

### Windows
- GHCup default: `C:\ghcup\` or `%APPDATA%\ghcup\`
- hx-managed: `%USERPROFILE%\.hx\toolchains\`

## Refreshing Detection

After installing tools, re-detect to get updated paths:

```rust
let mut toolchain = Toolchain::detect().await;

// Install missing GHC...
install_ghc("9.8.2").await?;

// Re-detect to find newly installed tool
toolchain = Toolchain::detect().await;
assert!(toolchain.ghc.status.is_found());
```
