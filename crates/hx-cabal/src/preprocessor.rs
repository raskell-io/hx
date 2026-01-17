//! Preprocessor integration for Haskell build tools.
//!
//! This module handles detection and invocation of preprocessors:
//! - **alex**: `.x` → `.hs` (lexer generator)
//! - **happy**: `.y` / `.ly` → `.hs` (parser generator)
//! - **hsc2hs**: `.hsc` → `.hs` (C FFI bindings)

use hx_core::{CommandRunner, Error, Fix, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::{debug, info, warn};

/// Supported preprocessor types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Preprocessor {
    /// Alex lexer generator (.x → .hs)
    Alex,
    /// Happy parser generator (.y / .ly → .hs)
    Happy,
    /// hsc2hs C FFI bindings (.hsc → .hs)
    Hsc2hs,
}

impl Preprocessor {
    /// Get the name of the preprocessor executable.
    pub fn executable(&self) -> &'static str {
        match self {
            Preprocessor::Alex => "alex",
            Preprocessor::Happy => "happy",
            Preprocessor::Hsc2hs => "hsc2hs",
        }
    }

    /// Get the source file extension(s) for this preprocessor.
    pub fn extensions(&self) -> &'static [&'static str] {
        match self {
            Preprocessor::Alex => &["x"],
            Preprocessor::Happy => &["y", "ly"],
            Preprocessor::Hsc2hs => &["hsc"],
        }
    }

    /// Infer preprocessor from file extension.
    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext.to_lowercase().as_str() {
            "x" => Some(Preprocessor::Alex),
            "y" | "ly" => Some(Preprocessor::Happy),
            "hsc" => Some(Preprocessor::Hsc2hs),
            _ => None,
        }
    }
}

impl std::fmt::Display for Preprocessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.executable())
    }
}

/// Configuration for running preprocessors.
#[derive(Debug, Clone, Default)]
pub struct PreprocessorConfig {
    /// Include directories for C headers (for hsc2hs)
    pub include_dirs: Vec<PathBuf>,
    /// CPP defines (for hsc2hs)
    pub cpp_defines: Vec<String>,
    /// Output directory for generated files
    pub output_dir: PathBuf,
    /// Enable verbose output
    pub verbose: bool,
    /// Extra C compiler flags (for hsc2hs)
    pub cc_options: Vec<String>,
    /// Extra linker flags (for hsc2hs)
    pub ld_options: Vec<String>,
}

/// Result of preprocessing a single file.
#[derive(Debug, Clone)]
pub struct PreprocessResult {
    /// Input file path
    pub input_path: PathBuf,
    /// Output file path (generated .hs file)
    pub output_path: PathBuf,
    /// Whether preprocessing succeeded
    pub success: bool,
    /// Module name inferred from the file
    pub module_name: Option<String>,
    /// Error messages if failed
    pub errors: Vec<String>,
    /// Warning messages
    pub warnings: Vec<String>,
    /// Duration of preprocessing
    pub duration: std::time::Duration,
}

/// Collection of source files that need preprocessing.
#[derive(Debug, Clone, Default)]
pub struct PreprocessorSources {
    /// Alex lexer files (.x)
    pub alex_files: Vec<PathBuf>,
    /// Happy parser files (.y, .ly)
    pub happy_files: Vec<PathBuf>,
    /// hsc2hs FFI files (.hsc)
    pub hsc_files: Vec<PathBuf>,
}

impl PreprocessorSources {
    /// Check if there are any files to preprocess.
    pub fn is_empty(&self) -> bool {
        self.alex_files.is_empty() && self.happy_files.is_empty() && self.hsc_files.is_empty()
    }

    /// Total number of files to preprocess.
    pub fn total_files(&self) -> usize {
        self.alex_files.len() + self.happy_files.len() + self.hsc_files.len()
    }

    /// Get all preprocessors needed for these sources.
    pub fn needed_preprocessors(&self) -> Vec<Preprocessor> {
        let mut needed = Vec::new();
        if !self.alex_files.is_empty() {
            needed.push(Preprocessor::Alex);
        }
        if !self.happy_files.is_empty() {
            needed.push(Preprocessor::Happy);
        }
        if !self.hsc_files.is_empty() {
            needed.push(Preprocessor::Hsc2hs);
        }
        needed
    }

    /// Get all files as an iterator with their preprocessor types.
    pub fn all_files(&self) -> impl Iterator<Item = (Preprocessor, &PathBuf)> {
        self.alex_files
            .iter()
            .map(|p| (Preprocessor::Alex, p))
            .chain(self.happy_files.iter().map(|p| (Preprocessor::Happy, p)))
            .chain(self.hsc_files.iter().map(|p| (Preprocessor::Hsc2hs, p)))
    }
}

/// Detect preprocessor source files in the given directories.
pub fn detect_preprocessors(source_dirs: &[PathBuf]) -> PreprocessorSources {
    let mut sources = PreprocessorSources::default();

    for dir in source_dirs {
        if !dir.exists() {
            continue;
        }

        scan_directory_for_preprocessor_files(dir, &mut sources);
    }

    debug!(
        "Detected preprocessor files: {} alex, {} happy, {} hsc2hs",
        sources.alex_files.len(),
        sources.happy_files.len(),
        sources.hsc_files.len()
    );

    sources
}

/// Recursively scan a directory for preprocessor source files.
fn scan_directory_for_preprocessor_files(dir: &Path, sources: &mut PreprocessorSources) {
    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();

        if path.is_dir() {
            scan_directory_for_preprocessor_files(&path, sources);
        } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            match ext.to_lowercase().as_str() {
                "x" => sources.alex_files.push(path),
                "y" | "ly" => sources.happy_files.push(path),
                "hsc" => sources.hsc_files.push(path),
                _ => {}
            }
        }
    }
}

/// Check which preprocessors are available in the system.
pub async fn check_availability(
    needed: &PreprocessorSources,
) -> Result<HashMap<Preprocessor, PathBuf>> {
    let mut available = HashMap::new();
    let runner = CommandRunner::new();

    for preprocessor in needed.needed_preprocessors() {
        let exe = preprocessor.executable();

        // Try to find the executable
        let output = match runner.run("which", [exe]).await {
            Ok(o) if o.success() => o,
            _ => {
                // On Windows, try where instead
                match runner.run("where", [exe]).await {
                    Ok(o) if o.success() => o,
                    _ => continue,
                }
            }
        };

        let path = output.stdout.lines().next().map(PathBuf::from);
        if let Some(p) = path {
            debug!("Found {}: {}", exe, p.display());
            available.insert(preprocessor, p);
        }
    }

    // Check if all needed preprocessors are available
    let needed_list = needed.needed_preprocessors();
    let missing: Vec<_> = needed_list
        .iter()
        .filter(|p| !available.contains_key(p))
        .collect();

    if !missing.is_empty() {
        let missing_names: Vec<_> = missing.iter().map(|p| p.executable()).collect();
        return Err(Error::ToolchainMissing {
            tool: missing_names.join(", "),
            source: None,
            fixes: vec![
                Fix::with_command(
                    "Install missing preprocessors with cabal",
                    format!("cabal install {}", missing_names.join(" ")),
                ),
                Fix::new("Or install via your system package manager"),
            ],
        });
    }

    Ok(available)
}

/// Run the Alex lexer generator.
pub async fn run_alex(
    input: &Path,
    output_dir: &Path,
    config: &PreprocessorConfig,
) -> Result<PreprocessResult> {
    let start = Instant::now();

    // Compute output path
    let output_path = compute_output_path(input, output_dir, "hs");

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| Error::Io {
            message: "failed to create output directory".to_string(),
            path: Some(parent.to_path_buf()),
            source: e,
        })?;
    }

    let mut args = vec![
        "-o".to_string(),
        output_path.display().to_string(),
        "-g".to_string(), // Generate GHC-compatible code
    ];

    args.push(input.display().to_string());

    if config.verbose {
        info!("Running alex: alex {}", args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run("alex", args_refs).await?;

    let success = cmd_output.success();
    let (warnings, errors) = parse_preprocessor_output(&cmd_output.stderr, &cmd_output.stdout);

    Ok(PreprocessResult {
        input_path: input.to_path_buf(),
        output_path,
        success,
        module_name: infer_module_name(input),
        errors,
        warnings,
        duration: start.elapsed(),
    })
}

/// Run the Happy parser generator.
pub async fn run_happy(
    input: &Path,
    output_dir: &Path,
    config: &PreprocessorConfig,
) -> Result<PreprocessResult> {
    let start = Instant::now();

    // Compute output path
    let output_path = compute_output_path(input, output_dir, "hs");

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| Error::Io {
            message: "failed to create output directory".to_string(),
            path: Some(parent.to_path_buf()),
            source: e,
        })?;
    }

    let mut args = vec![
        "-o".to_string(),
        output_path.display().to_string(),
        "-g".to_string(),  // Generate GHC-compatible code
        "-a".to_string(),  // Use arrays (more efficient)
        "-c".to_string(),  // Generate GHC-compatible code
    ];

    args.push(input.display().to_string());

    if config.verbose {
        info!("Running happy: happy {}", args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run("happy", args_refs).await?;

    let success = cmd_output.success();
    let (warnings, errors) = parse_preprocessor_output(&cmd_output.stderr, &cmd_output.stdout);

    Ok(PreprocessResult {
        input_path: input.to_path_buf(),
        output_path,
        success,
        module_name: infer_module_name(input),
        errors,
        warnings,
        duration: start.elapsed(),
    })
}

/// Run the hsc2hs preprocessor.
pub async fn run_hsc2hs(
    input: &Path,
    output_dir: &Path,
    config: &PreprocessorConfig,
    ghc_path: &Path,
) -> Result<PreprocessResult> {
    let start = Instant::now();

    // Compute output path
    let output_path = compute_output_path(input, output_dir, "hs");

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| Error::Io {
            message: "failed to create output directory".to_string(),
            path: Some(parent.to_path_buf()),
            source: e,
        })?;
    }

    let mut args = vec![
        "-o".to_string(),
        output_path.display().to_string(),
    ];

    // Add compiler flags
    args.push(format!("--cc={}", ghc_path.display()));

    // Add include directories
    for inc_dir in &config.include_dirs {
        args.push(format!("-I{}", inc_dir.display()));
    }

    // Add CPP defines
    for define in &config.cpp_defines {
        args.push(format!("-D{}", define));
    }

    // Add CC options
    for opt in &config.cc_options {
        args.push(format!("--cflag={}", opt));
    }

    // Add LD options
    for opt in &config.ld_options {
        args.push(format!("--lflag={}", opt));
    }

    args.push(input.display().to_string());

    if config.verbose {
        info!("Running hsc2hs: hsc2hs {}", args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run("hsc2hs", args_refs).await?;

    let success = cmd_output.success();
    let (warnings, errors) = parse_preprocessor_output(&cmd_output.stderr, &cmd_output.stdout);

    Ok(PreprocessResult {
        input_path: input.to_path_buf(),
        output_path,
        success,
        module_name: infer_module_name(input),
        errors,
        warnings,
        duration: start.elapsed(),
    })
}

/// Run all preprocessors on the given sources.
pub async fn preprocess_all(
    sources: &PreprocessorSources,
    config: &PreprocessorConfig,
    ghc_path: &Path,
) -> Result<Vec<PreprocessResult>> {
    if sources.is_empty() {
        return Ok(Vec::new());
    }

    info!(
        "Preprocessing {} files ({} alex, {} happy, {} hsc2hs)",
        sources.total_files(),
        sources.alex_files.len(),
        sources.happy_files.len(),
        sources.hsc_files.len()
    );

    let mut results = Vec::new();
    let mut had_errors = false;

    // Process alex files
    for file in &sources.alex_files {
        let result = run_alex(file, &config.output_dir, config).await?;
        if !result.success {
            had_errors = true;
            for error in &result.errors {
                warn!("alex error in {}: {}", file.display(), error);
            }
        }
        results.push(result);
    }

    // Process happy files
    for file in &sources.happy_files {
        let result = run_happy(file, &config.output_dir, config).await?;
        if !result.success {
            had_errors = true;
            for error in &result.errors {
                warn!("happy error in {}: {}", file.display(), error);
            }
        }
        results.push(result);
    }

    // Process hsc2hs files
    for file in &sources.hsc_files {
        let result = run_hsc2hs(file, &config.output_dir, config, ghc_path).await?;
        if !result.success {
            had_errors = true;
            for error in &result.errors {
                warn!("hsc2hs error in {}: {}", file.display(), error);
            }
        }
        results.push(result);
    }

    if had_errors {
        let error_count = results.iter().filter(|r| !r.success).count();
        return Err(Error::BuildFailed {
            errors: vec![format!("{} preprocessor(s) failed", error_count)],
            fixes: vec![Fix::with_command(
                "Run with verbose output for details",
                "hx build --verbose",
            )],
        });
    }

    info!(
        "Preprocessed {} files successfully",
        results.len()
    );

    Ok(results)
}

/// Compute the output path for a preprocessed file.
fn compute_output_path(input: &Path, output_dir: &Path, new_ext: &str) -> PathBuf {
    let stem = input.file_stem().unwrap_or_default();

    // Preserve directory structure relative to the source
    // e.g., src/Lexer.x -> generated/src/Lexer.hs
    let relative_dir = input.parent().unwrap_or(Path::new(""));

    output_dir
        .join(relative_dir)
        .join(format!("{}.{}", stem.to_string_lossy(), new_ext))
}

/// Infer module name from file path.
fn infer_module_name(path: &Path) -> Option<String> {
    let stem = path.file_stem()?.to_str()?;

    // Try to infer from directory structure (e.g., src/Data/Text/Lexer.x -> Data.Text.Lexer)
    let mut parts = Vec::new();

    for component in path.components() {
        if let std::path::Component::Normal(s) = component {
            let s = s.to_string_lossy();
            // Skip common source directory names
            if s != "src" && s != "lib" && s != "app" && s != "." {
                // Check if this looks like a module component (starts with uppercase)
                if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    // Remove extension if present
                    let name = s.split('.').next().unwrap_or(&s);
                    parts.push(name.to_string());
                }
            }
        }
    }

    if parts.is_empty() {
        // Just use the filename
        Some(stem.to_string())
    } else {
        Some(parts.join("."))
    }
}

/// Parse preprocessor output into warnings and errors.
fn parse_preprocessor_output(stderr: &str, stdout: &str) -> (Vec<String>, Vec<String>) {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    let combined = format!("{}\n{}", stderr, stdout);

    for line in combined.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let lower = trimmed.to_lowercase();
        if lower.contains("error:") || lower.contains("error ")
            || lower.starts_with("parse error")
            || lower.contains("cannot")
            || lower.contains("failed")
        {
            errors.push(trimmed.to_string());
        } else if lower.contains("warning:") || lower.contains("warning ") {
            warnings.push(trimmed.to_string());
        }
    }

    (warnings, errors)
}

/// Check if a build-tools list indicates preprocessor requirements.
pub fn parse_build_tools(tools: &[String]) -> Vec<Preprocessor> {
    let mut preprocessors = Vec::new();

    for tool in tools {
        let tool_lower = tool.to_lowercase();

        // Handle both "alex" and "alex:alex >= 3.0" formats
        let tool_name = tool_lower.split(':').next().unwrap_or(&tool_lower);
        let tool_name = tool_name.split_whitespace().next().unwrap_or(tool_name);

        match tool_name {
            "alex" => preprocessors.push(Preprocessor::Alex),
            "happy" => preprocessors.push(Preprocessor::Happy),
            "hsc2hs" => preprocessors.push(Preprocessor::Hsc2hs),
            _ => {}
        }
    }

    preprocessors
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocessor_from_extension() {
        assert_eq!(Preprocessor::from_extension("x"), Some(Preprocessor::Alex));
        assert_eq!(Preprocessor::from_extension("y"), Some(Preprocessor::Happy));
        assert_eq!(Preprocessor::from_extension("ly"), Some(Preprocessor::Happy));
        assert_eq!(Preprocessor::from_extension("hsc"), Some(Preprocessor::Hsc2hs));
        assert_eq!(Preprocessor::from_extension("hs"), None);
        assert_eq!(Preprocessor::from_extension("X"), Some(Preprocessor::Alex));
    }

    #[test]
    fn test_preprocessor_sources_is_empty() {
        let sources = PreprocessorSources::default();
        assert!(sources.is_empty());

        let sources = PreprocessorSources {
            alex_files: vec![PathBuf::from("test.x")],
            ..Default::default()
        };
        assert!(!sources.is_empty());
    }

    #[test]
    fn test_preprocessor_sources_total_files() {
        let sources = PreprocessorSources {
            alex_files: vec![PathBuf::from("a.x"), PathBuf::from("b.x")],
            happy_files: vec![PathBuf::from("c.y")],
            hsc_files: vec![PathBuf::from("d.hsc"), PathBuf::from("e.hsc")],
        };
        assert_eq!(sources.total_files(), 5);
    }

    #[test]
    fn test_needed_preprocessors() {
        let sources = PreprocessorSources {
            alex_files: vec![PathBuf::from("a.x")],
            happy_files: vec![],
            hsc_files: vec![PathBuf::from("b.hsc")],
        };

        let needed = sources.needed_preprocessors();
        assert_eq!(needed.len(), 2);
        assert!(needed.contains(&Preprocessor::Alex));
        assert!(!needed.contains(&Preprocessor::Happy));
        assert!(needed.contains(&Preprocessor::Hsc2hs));
    }

    #[test]
    fn test_compute_output_path() {
        let input = PathBuf::from("src/Lexer.x");
        let output_dir = PathBuf::from("generated");
        let output = compute_output_path(&input, &output_dir, "hs");
        assert_eq!(output, PathBuf::from("generated/src/Lexer.hs"));
    }

    #[test]
    fn test_infer_module_name() {
        assert_eq!(
            infer_module_name(Path::new("src/Data/Text/Lexer.x")),
            Some("Data.Text.Lexer".to_string())
        );
        assert_eq!(
            infer_module_name(Path::new("Lexer.x")),
            Some("Lexer".to_string())
        );
        assert_eq!(
            infer_module_name(Path::new("src/lexer.x")),
            Some("lexer".to_string())
        );
    }

    #[test]
    fn test_parse_build_tools() {
        let tools = vec![
            "alex".to_string(),
            "happy:happy >= 1.19".to_string(),
            "some-other-tool".to_string(),
        ];

        let preprocessors = parse_build_tools(&tools);
        assert_eq!(preprocessors.len(), 2);
        assert!(preprocessors.contains(&Preprocessor::Alex));
        assert!(preprocessors.contains(&Preprocessor::Happy));
    }

    #[test]
    fn test_parse_preprocessor_output() {
        let stderr = "Warning: rule never reduced\nerror: parse error on line 5";
        let stdout = "";

        let (warnings, errors) = parse_preprocessor_output(stderr, stdout);
        assert_eq!(warnings.len(), 1);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_preprocessor_display() {
        assert_eq!(format!("{}", Preprocessor::Alex), "alex");
        assert_eq!(format!("{}", Preprocessor::Happy), "happy");
        assert_eq!(format!("{}", Preprocessor::Hsc2hs), "hsc2hs");
    }
}
