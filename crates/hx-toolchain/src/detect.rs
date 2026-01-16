//! Tool detection utilities.

use crate::ghc::{GhcSource, ResolutionConfig, resolve_ghc};
use hx_core::{CommandRunner, Version};
use std::path::PathBuf;
use tracing::{debug, instrument};

/// Status of a detected tool.
#[derive(Debug, Clone)]
pub enum ToolStatus {
    /// Tool found with version
    Found { version: Version, path: PathBuf },
    /// Tool not found
    NotFound,
    /// Tool found but version couldn't be parsed
    VersionUnknown { path: PathBuf },
}

impl ToolStatus {
    /// Check if the tool was found.
    pub fn is_found(&self) -> bool {
        matches!(
            self,
            ToolStatus::Found { .. } | ToolStatus::VersionUnknown { .. }
        )
    }

    /// Get the version if available.
    pub fn version(&self) -> Option<&Version> {
        match self {
            ToolStatus::Found { version, .. } => Some(version),
            _ => None,
        }
    }

    /// Get the path if available.
    pub fn path(&self) -> Option<&PathBuf> {
        match self {
            ToolStatus::Found { path, .. } | ToolStatus::VersionUnknown { path } => Some(path),
            ToolStatus::NotFound => None,
        }
    }
}

/// A detected tool.
#[derive(Debug, Clone)]
pub struct DetectedTool {
    /// Tool name
    pub name: String,
    /// Detection status
    pub status: ToolStatus,
    /// Source of the tool (hx-managed, ghcup, system)
    pub source: Option<GhcSource>,
}

/// Detected toolchain state.
#[derive(Debug, Clone)]
pub struct Toolchain {
    /// GHC compiler
    pub ghc: DetectedTool,
    /// Cabal build tool
    pub cabal: DetectedTool,
    /// GHCup toolchain manager
    pub ghcup: DetectedTool,
    /// Haskell Language Server
    pub hls: DetectedTool,
}

impl Toolchain {
    /// Detect all tools asynchronously.
    #[instrument]
    pub async fn detect() -> Self {
        let runner = CommandRunner::new();

        // Run detections in parallel
        let (ghc, cabal, ghcup, hls) = tokio::join!(
            detect_ghc(&runner),
            detect_cabal(&runner),
            detect_ghcup(&runner),
            detect_hls(&runner),
        );

        Self {
            ghc,
            cabal,
            ghcup,
            hls,
        }
    }

    /// Check if all required tools are present.
    pub fn is_complete(&self) -> bool {
        self.ghc.status.is_found() && self.cabal.status.is_found()
    }

    /// Check if ghcup is available for toolchain management.
    pub fn has_ghcup(&self) -> bool {
        self.ghcup.status.is_found()
    }
}

async fn detect_ghc(runner: &CommandRunner) -> DetectedTool {
    // First, check hx-managed installations
    let config = ResolutionConfig::default();
    if let Ok(Some(resolved)) = resolve_ghc(&config) {
        debug!(
            "Found hx-managed GHC {} at {}",
            resolved.version,
            resolved.bin_dir.display()
        );

        // Verify the binary works
        let ghc_path = resolved.ghc_path();
        if ghc_path.exists() {
            if let Ok(version) = resolved.version.parse() {
                return DetectedTool {
                    name: "ghc".to_string(),
                    status: ToolStatus::Found {
                        version,
                        path: ghc_path,
                    },
                    source: Some(resolved.source),
                };
            }
        }
    }

    // Fall back to PATH detection
    detect_tool(runner, "ghc", &["--version"]).await
}

async fn detect_cabal(runner: &CommandRunner) -> DetectedTool {
    detect_tool(runner, "cabal", &["--version"]).await
}

async fn detect_ghcup(runner: &CommandRunner) -> DetectedTool {
    detect_tool(runner, "ghcup", &["--version"]).await
}

async fn detect_hls(runner: &CommandRunner) -> DetectedTool {
    detect_tool(runner, "haskell-language-server-wrapper", &["--version"]).await
}

async fn detect_tool(runner: &CommandRunner, name: &str, args: &[&str]) -> DetectedTool {
    debug!("Detecting {}", name);

    // First check if the tool exists
    let path = match which::which(name) {
        Ok(p) => p,
        Err(_) => {
            debug!("{} not found in PATH", name);
            return DetectedTool {
                name: name.to_string(),
                status: ToolStatus::NotFound,
                source: None,
            };
        }
    };

    // Determine source based on path
    let source = if path.to_string_lossy().contains(".ghcup") {
        Some(GhcSource::Ghcup)
    } else {
        Some(GhcSource::System)
    };

    // Try to get version
    match runner.run(name, args.iter().copied()).await {
        Ok(output) if output.success() => {
            let version_str = format!("{}{}", output.stdout, output.stderr);
            if let Some(version) = Version::parse_from_output(&version_str) {
                debug!("{} version: {}", name, version);
                DetectedTool {
                    name: name.to_string(),
                    status: ToolStatus::Found { version, path },
                    source,
                }
            } else {
                debug!(
                    "{} found but couldn't parse version from: {}",
                    name,
                    version_str.trim()
                );
                DetectedTool {
                    name: name.to_string(),
                    status: ToolStatus::VersionUnknown { path },
                    source,
                }
            }
        }
        Ok(output) => {
            debug!("{} returned non-zero exit code: {}", name, output.exit_code);
            DetectedTool {
                name: name.to_string(),
                status: ToolStatus::VersionUnknown { path },
                source,
            }
        }
        Err(e) => {
            debug!("{} error: {}", name, e);
            DetectedTool {
                name: name.to_string(),
                status: ToolStatus::NotFound,
                source: None,
            }
        }
    }
}
