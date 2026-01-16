//! Platform detection for GHC binary downloads.

use serde::{Deserialize, Serialize};

/// Supported platforms for GHC binaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Platform {
    /// x86_64 Linux (glibc)
    X86_64Linux,
    /// aarch64 Linux (glibc)
    Aarch64Linux,
    /// x86_64 macOS (Intel)
    X86_64Darwin,
    /// aarch64 macOS (Apple Silicon)
    Aarch64Darwin,
    /// x86_64 Windows (MinGW)
    X86_64Windows,
}

impl Platform {
    /// Detect the current platform.
    pub fn current() -> Option<Self> {
        let arch = std::env::consts::ARCH;
        let os = std::env::consts::OS;

        match (arch, os) {
            ("x86_64", "linux") => Some(Self::X86_64Linux),
            ("aarch64", "linux") => Some(Self::Aarch64Linux),
            ("x86_64", "macos") => Some(Self::X86_64Darwin),
            ("aarch64", "macos") => Some(Self::Aarch64Darwin),
            ("x86_64", "windows") => Some(Self::X86_64Windows),
            _ => None,
        }
    }

    /// Get the URL suffix for this platform.
    ///
    /// This is used to construct download URLs like:
    /// `https://downloads.haskell.org/~ghc/9.8.2/ghc-9.8.2-{suffix}.tar.xz`
    pub fn url_suffix(&self) -> &'static str {
        match self {
            Self::X86_64Linux => "x86_64-unknown-linux",
            Self::Aarch64Linux => "aarch64-unknown-linux",
            Self::X86_64Darwin => "x86_64-apple-darwin",
            Self::Aarch64Darwin => "aarch64-apple-darwin",
            Self::X86_64Windows => "x86_64-unknown-mingw32",
        }
    }

    /// Get the archive extension for this platform.
    pub fn archive_extension(&self) -> &'static str {
        // All platforms use .tar.xz for GHC binaries
        "xz"
    }

    /// Get a human-readable display name.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::X86_64Linux => "x86_64-linux",
            Self::Aarch64Linux => "aarch64-linux",
            Self::X86_64Darwin => "x86_64-macos",
            Self::Aarch64Darwin => "aarch64-macos (Apple Silicon)",
            Self::X86_64Windows => "x86_64-windows",
        }
    }

    /// Check if this platform uses Unix conventions.
    pub fn is_unix(&self) -> bool {
        !matches!(self, Self::X86_64Windows)
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

/// Construct the GHC download URL for a version and platform.
pub fn ghc_download_url(version: &str, platform: Platform) -> String {
    format!(
        "https://downloads.haskell.org/~ghc/{version}/ghc-{version}-{suffix}.tar.{ext}",
        version = version,
        suffix = platform.url_suffix(),
        ext = platform.archive_extension()
    )
}

/// Construct the expected archive filename.
pub fn ghc_archive_filename(version: &str, platform: Platform) -> String {
    format!(
        "ghc-{version}-{suffix}.tar.{ext}",
        version = version,
        suffix = platform.url_suffix(),
        ext = platform.archive_extension()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_detection() {
        // This test will pass on supported platforms
        let platform = Platform::current();
        // We should get Some on CI/dev machines
        if cfg!(target_os = "linux") || cfg!(target_os = "macos") || cfg!(target_os = "windows") {
            assert!(platform.is_some());
        }
    }

    #[test]
    fn test_url_suffix() {
        assert_eq!(Platform::X86_64Linux.url_suffix(), "x86_64-unknown-linux");
        assert_eq!(Platform::Aarch64Darwin.url_suffix(), "aarch64-apple-darwin");
        assert_eq!(Platform::X86_64Windows.url_suffix(), "x86_64-unknown-mingw32");
    }

    #[test]
    fn test_ghc_download_url() {
        let url = ghc_download_url("9.8.2", Platform::X86_64Darwin);
        assert_eq!(
            url,
            "https://downloads.haskell.org/~ghc/9.8.2/ghc-9.8.2-x86_64-apple-darwin.tar.xz"
        );

        let url = ghc_download_url("9.6.4", Platform::Aarch64Linux);
        assert_eq!(
            url,
            "https://downloads.haskell.org/~ghc/9.6.4/ghc-9.6.4-aarch64-unknown-linux.tar.xz"
        );
    }

    #[test]
    fn test_is_unix() {
        assert!(Platform::X86_64Linux.is_unix());
        assert!(Platform::X86_64Darwin.is_unix());
        assert!(!Platform::X86_64Windows.is_unix());
    }
}
