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
///
/// The `ghc_path` parameter is used to check the GHC bin directory for tools
/// like hsc2hs that are bundled with GHC and may not be in PATH.
pub async fn check_availability(
    needed: &PreprocessorSources,
    ghc_path: Option<&Path>,
) -> Result<HashMap<Preprocessor, PathBuf>> {
    let mut available = HashMap::new();
    let runner = CommandRunner::new();

    // Get the GHC bin directory if available
    let ghc_bin_dir = ghc_path.and_then(|p| p.parent());

    for preprocessor in needed.needed_preprocessors() {
        let exe = preprocessor.executable();

        // First, check the GHC bin directory (for hsc2hs and other bundled tools)
        if let Some(bin_dir) = ghc_bin_dir {
            let tool_path = bin_dir.join(exe);
            if tool_path.exists() {
                debug!("Found {} in GHC bin dir: {}", exe, tool_path.display());
                available.insert(preprocessor, tool_path);
                continue;
            }
        }

        // Try to find the executable in PATH
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
        let file_types: Vec<_> = missing
            .iter()
            .flat_map(|p| p.extensions().iter().map(|e| format!(".{}", e)))
            .collect();

        // Build a helpful message about what files require these tools
        let files_requiring: Vec<String> = missing
            .iter()
            .filter_map(|p| {
                let files = match p {
                    Preprocessor::Alex => &needed.alex_files,
                    Preprocessor::Happy => &needed.happy_files,
                    Preprocessor::Hsc2hs => &needed.hsc_files,
                };
                if files.is_empty() {
                    None
                } else {
                    Some(format!("{}: {} file(s)", p.executable(), files.len()))
                }
            })
            .collect();

        let mut fixes = vec![Fix::with_command(
            "Install missing preprocessors with cabal",
            format!("cabal install {}", missing_names.join(" ")),
        )];

        // Add platform-specific installation hints
        #[cfg(target_os = "macos")]
        fixes.push(Fix::with_command(
            "Or install via Homebrew",
            format!("brew install ghc  # includes {}", missing_names.join(", ")),
        ));

        #[cfg(target_os = "linux")]
        fixes.push(Fix::new(
            "Or install via your system package manager (e.g., apt install alex happy)",
        ));

        fixes.push(Fix::new(format!(
            "These tools are needed to compile {} files",
            file_types.join(", ")
        )));

        return Err(Error::ToolchainMissing {
            tool: format!(
                "{} (required for {})",
                missing_names.join(", "),
                files_requiring.join(", ")
            ),
            source: None,
            fixes,
        });
    }

    Ok(available)
}

/// Run the Alex lexer generator.
pub async fn run_alex(
    input: &Path,
    output_dir: &Path,
    config: &PreprocessorConfig,
    exe_path: &Path,
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

    let exe_str = exe_path.display().to_string();
    if config.verbose {
        info!("Running alex: {} {}", exe_str, args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run(exe_str.as_str(), args_refs).await?;

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
    exe_path: &Path,
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
        "-a".to_string(), // Use arrays (more efficient)
        "-c".to_string(), // Generate GHC-compatible code
    ];

    args.push(input.display().to_string());

    let exe_str = exe_path.display().to_string();
    if config.verbose {
        info!("Running happy: {} {}", exe_str, args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run(exe_str.as_str(), args_refs).await?;

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
    _ghc_path: &Path,
    exe_path: &Path,
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

    let mut args = vec!["-o".to_string(), output_path.display().to_string()];

    // Note: We don't set --cc here; hsc2hs will use the default C compiler.
    // Setting --cc to GHC doesn't work as GHC doesn't accept raw C compiler flags.

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

    let exe_str = exe_path.display().to_string();
    if config.verbose {
        info!("Running hsc2hs: {} {}", exe_str, args.join(" "));
    }

    let runner = CommandRunner::new();
    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run(exe_str.as_str(), args_refs).await?;

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
///
/// The `available` map should contain the paths to each preprocessor executable,
/// as returned by `check_availability`.
pub async fn preprocess_all(
    sources: &PreprocessorSources,
    config: &PreprocessorConfig,
    ghc_path: &Path,
    available: &HashMap<Preprocessor, PathBuf>,
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
    if let Some(alex_path) = available.get(&Preprocessor::Alex) {
        for file in &sources.alex_files {
            let result = run_alex(file, &config.output_dir, config, alex_path).await?;
            if !result.success {
                had_errors = true;
                for error in &result.errors {
                    warn!("alex error in {}: {}", file.display(), error);
                }
            }
            results.push(result);
        }
    }

    // Process happy files
    if let Some(happy_path) = available.get(&Preprocessor::Happy) {
        for file in &sources.happy_files {
            let result = run_happy(file, &config.output_dir, config, happy_path).await?;
            if !result.success {
                had_errors = true;
                for error in &result.errors {
                    warn!("happy error in {}: {}", file.display(), error);
                }
            }
            results.push(result);
        }
    }

    // Process hsc2hs files
    if let Some(hsc2hs_path) = available.get(&Preprocessor::Hsc2hs) {
        for file in &sources.hsc_files {
            let result =
                run_hsc2hs(file, &config.output_dir, config, ghc_path, hsc2hs_path).await?;
            if !result.success {
                had_errors = true;
                for error in &result.errors {
                    warn!("hsc2hs error in {}: {}", file.display(), error);
                }
            }
            results.push(result);
        }
    }

    if had_errors {
        let failed_results: Vec<_> = results.iter().filter(|r| !r.success).collect();
        let error_count = failed_results.len();

        // Collect detailed error information
        let mut detailed_errors = Vec::new();
        for result in &failed_results {
            let file_name = result
                .input_path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_else(|| result.input_path.display().to_string());

            if result.errors.is_empty() {
                detailed_errors.push(format!("{}: preprocessing failed", file_name));
            } else {
                for err in &result.errors {
                    detailed_errors.push(format!("{}: {}", file_name, err));
                }
            }
        }

        // Limit to first 5 errors to avoid overwhelming output
        let shown_errors: Vec<_> = detailed_errors.iter().take(5).cloned().collect();
        let hidden_count = detailed_errors.len().saturating_sub(5);

        let mut errors = shown_errors;
        if hidden_count > 0 {
            errors.push(format!("... and {} more error(s)", hidden_count));
        }

        // Build actionable fixes based on failure type
        let mut fixes = vec![Fix::with_command(
            "Run with verbose output for full details",
            "hx build --verbose",
        )];

        // Check if any hsc2hs files failed (common issue: missing C headers)
        let hsc_failed = failed_results
            .iter()
            .any(|r| r.input_path.extension().is_some_and(|e| e == "hsc"));
        if hsc_failed {
            fixes.push(Fix::new(
                "For hsc2hs errors: ensure required C headers and libraries are installed",
            ));
        }

        return Err(Error::BuildFailed {
            errors: vec![format!(
                "{} preprocessor file(s) failed to compile:\n  {}",
                error_count,
                errors.join("\n  ")
            )],
            fixes,
        });
    }

    info!("Preprocessed {} files successfully", results.len());

    Ok(results)
}

/// Compute the output path for a preprocessed file.
fn compute_output_path(input: &Path, output_dir: &Path, new_ext: &str) -> PathBuf {
    let stem = input.file_stem().unwrap_or_default();
    let parent = input.parent().unwrap_or(Path::new(""));

    // If the input path is absolute, we can't safely join it to output_dir
    // (on Unix, joining an absolute path replaces the base entirely).
    // In this case, just use the filename without preserving directory structure.
    let relative_dir = if parent.is_absolute() {
        Path::new("")
    } else {
        parent
    };

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
        if lower.contains("error:")
            || lower.contains("error ")
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
        assert_eq!(
            Preprocessor::from_extension("ly"),
            Some(Preprocessor::Happy)
        );
        assert_eq!(
            Preprocessor::from_extension("hsc"),
            Some(Preprocessor::Hsc2hs)
        );
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

    #[test]
    fn test_preprocessor_executable() {
        assert_eq!(Preprocessor::Alex.executable(), "alex");
        assert_eq!(Preprocessor::Happy.executable(), "happy");
        assert_eq!(Preprocessor::Hsc2hs.executable(), "hsc2hs");
    }

    #[test]
    fn test_preprocessor_extensions() {
        assert_eq!(Preprocessor::Alex.extensions(), &["x"]);
        assert_eq!(Preprocessor::Happy.extensions(), &["y", "ly"]);
        assert_eq!(Preprocessor::Hsc2hs.extensions(), &["hsc"]);
    }

    #[test]
    fn test_preprocessor_sources_all_files() {
        let sources = PreprocessorSources {
            alex_files: vec![PathBuf::from("a.x")],
            happy_files: vec![PathBuf::from("b.y")],
            hsc_files: vec![PathBuf::from("c.hsc")],
        };

        let all: Vec<_> = sources.all_files().collect();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn test_detect_preprocessors_in_temp_dir() {
        let temp_dir = tempfile::tempdir().unwrap();

        // Create test files
        std::fs::write(temp_dir.path().join("Lexer.x"), "-- alex file").unwrap();
        std::fs::write(temp_dir.path().join("Parser.y"), "-- happy file").unwrap();
        std::fs::create_dir(temp_dir.path().join("subdir")).unwrap();
        std::fs::write(temp_dir.path().join("subdir/FFI.hsc"), "-- hsc2hs file").unwrap();

        let sources = detect_preprocessors(&[temp_dir.path().to_path_buf()]);

        assert_eq!(sources.alex_files.len(), 1);
        assert_eq!(sources.happy_files.len(), 1);
        assert_eq!(sources.hsc_files.len(), 1);
    }

    #[test]
    fn test_detect_preprocessors_empty_dir() {
        let temp_dir = tempfile::tempdir().unwrap();
        let sources = detect_preprocessors(&[temp_dir.path().to_path_buf()]);
        assert!(sources.is_empty());
    }

    #[test]
    fn test_detect_preprocessors_nonexistent_dir() {
        let sources = detect_preprocessors(&[PathBuf::from("/nonexistent/path")]);
        assert!(sources.is_empty());
    }

    #[test]
    fn test_compute_output_path_nested() {
        let input = PathBuf::from("src/Data/Internal/Lexer.x");
        let output_dir = PathBuf::from("dist/generated");
        let output = compute_output_path(&input, &output_dir, "hs");
        assert_eq!(
            output,
            PathBuf::from("dist/generated/src/Data/Internal/Lexer.hs")
        );
    }

    #[test]
    fn test_compute_output_path_root_file() {
        let input = PathBuf::from("Main.x");
        let output_dir = PathBuf::from("generated");
        let output = compute_output_path(&input, &output_dir, "hs");
        assert_eq!(output, PathBuf::from("generated/Main.hs"));
    }

    #[test]
    #[cfg(not(windows))] // Unix-style absolute paths don't work the same on Windows
    fn test_compute_output_path_absolute_input() {
        // When input has an absolute path, we should not try to join it
        // as that would replace the output_dir entirely on Unix
        let input = PathBuf::from("/home/user/project/src/Lexer.x");
        let output_dir = PathBuf::from("/home/user/project/.hx/generated");
        let output = compute_output_path(&input, &output_dir, "hs");
        // Should just use the filename, not the full absolute path
        assert_eq!(
            output,
            PathBuf::from("/home/user/project/.hx/generated/Lexer.hs")
        );
    }

    #[test]
    fn test_parse_preprocessor_output_mixed() {
        let stderr = r#"
Warning: unused rule
error: undefined symbol 'foo'
Warning: deprecated feature
Cannot find module
"#;
        let stdout = "Processing file...";

        let (warnings, errors) = parse_preprocessor_output(stderr, stdout);
        assert_eq!(warnings.len(), 2);
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn test_parse_preprocessor_output_empty() {
        let (warnings, errors) = parse_preprocessor_output("", "");
        assert!(warnings.is_empty());
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_build_tools_with_version_constraints() {
        let tools = vec![
            "alex >= 3.0".to_string(),
            "happy:happy >= 1.19 && < 2.0".to_string(),
            "hsc2hs".to_string(),
        ];

        let preprocessors = parse_build_tools(&tools);
        assert_eq!(preprocessors.len(), 3);
        assert!(preprocessors.contains(&Preprocessor::Alex));
        assert!(preprocessors.contains(&Preprocessor::Happy));
        assert!(preprocessors.contains(&Preprocessor::Hsc2hs));
    }

    #[test]
    fn test_preprocessor_config_default() {
        let config = PreprocessorConfig::default();
        assert!(config.include_dirs.is_empty());
        assert!(config.cpp_defines.is_empty());
        assert!(!config.verbose);
    }

    #[test]
    fn test_preprocess_result_fields() {
        let result = PreprocessResult {
            input_path: PathBuf::from("test.x"),
            output_path: PathBuf::from("test.hs"),
            success: true,
            module_name: Some("Test".to_string()),
            errors: vec![],
            warnings: vec!["warning".to_string()],
            duration: std::time::Duration::from_millis(100),
        };

        assert!(result.success);
        assert_eq!(result.module_name, Some("Test".to_string()));
        assert_eq!(result.warnings.len(), 1);
    }
}
