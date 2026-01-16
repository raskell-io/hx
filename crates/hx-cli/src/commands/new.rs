//! Create new modules and files from templates.

use anyhow::{Context, Result};
use hx_config::find_project_root;
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};
use std::fs;
use std::path::{Path, PathBuf};

use crate::cli::NewCommands;
use crate::templates::{self, TemplateContext, TemplateFile};

/// Run the new command.
pub async fn run(command: NewCommands, output: &Output) -> Result<i32> {
    match command {
        NewCommands::Module { name, src: _, test } => {
            let dir = if test { "test" } else { "src" };
            create_module(&name, dir, output)
        }
        NewCommands::Test { name } => create_test_module(&name, output),
        NewCommands::Benchmark { name } => create_benchmark_module(&name, output),
        NewCommands::Webapp { name, dir } => create_project_from_template(&name, dir, "webapp", output),
        NewCommands::Cli { name, dir } => create_project_from_template(&name, dir, "cli", output),
        NewCommands::Library { name, dir } => create_project_from_template(&name, dir, "library", output),
        NewCommands::Template { url, name, dir, branch } => {
            create_project_from_git_template(&url, &name, dir, &branch, output).await
        }
    }
}

fn create_project_from_template(
    name: &str,
    dir: Option<String>,
    template: &str,
    output: &Output,
) -> Result<i32> {
    let target_dir = dir.unwrap_or_else(|| name.to_string());
    let target_path = Path::new(&target_dir);

    // Check if directory already exists and is not empty
    if target_path.exists() {
        let entries: Vec<_> = fs::read_dir(target_path)?.collect();
        if !entries.is_empty() {
            output.error(&format!(
                "Directory '{}' already exists and is not empty",
                target_dir
            ));
            return Ok(1);
        }
    }

    // Create directory
    fs::create_dir_all(target_path)
        .with_context(|| format!("Failed to create directory: {}", target_dir))?;

    output.status("Creating", &format!("{} project '{}'", template, name));

    // Get template files
    let files: &[TemplateFile] = match template {
        "webapp" => templates::webapp::FILES,
        "cli" => templates::cli::FILES,
        "library" => templates::library::FILES,
        _ => unreachable!("Unknown template: {}", template),
    };

    // Create context for variable substitution
    let ctx = TemplateContext::new(name);

    // Write all template files
    for file in files {
        // Substitute variables in the path as well
        let path_str = ctx.substitute(file.path);
        let file_path = target_path.join(&path_str);

        // Create parent directories
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
        }

        // Write file with variable substitution
        let content = ctx.substitute(file.content);
        fs::write(&file_path, &content)
            .with_context(|| format!("Failed to write: {}", file_path.display()))?;

        output.verbose(&format!("  Created {}", path_str));
    }

    // Initialize git repository
    if let Err(e) = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(target_path)
        .status()
    {
        output.verbose(&format!("Note: git init failed: {}", e));
    }

    output.status("Created", &format!("{} project in '{}'", template, target_dir));
    output.info("");
    output.info("Next steps:");
    output.info(&format!("  cd {}", target_dir));
    output.info("  hx build");

    if template == "webapp" {
        output.info("  hx run  # starts server on http://localhost:8080");
    } else if template == "cli" {
        output.info("  hx run -- --help");
    }

    Ok(0)
}

fn create_module(name: &str, base_dir: &str, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    // Convert module name to file path (e.g., Data.List.Extra -> Data/List/Extra.hs)
    let relative_path = module_name_to_path(name);
    let full_path = project_root.join(base_dir).join(&relative_path);

    // Check if file already exists
    if full_path.exists() {
        output.error(&format!("Module already exists: {}", full_path.display()));
        return Ok(1);
    }

    // Create parent directories
    if let Some(parent) = full_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
    }

    // Generate module content
    let content = generate_module_content(name);

    // Write the file
    fs::write(&full_path, content)
        .with_context(|| format!("Failed to write file: {}", full_path.display()))?;

    output.status(
        "Created",
        &format!("{}/{}", base_dir, relative_path.display()),
    );

    Ok(0)
}

fn create_test_module(name: &str, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    let relative_path = module_name_to_path(name);
    let full_path = project_root.join("test").join(&relative_path);

    if full_path.exists() {
        output.error(&format!(
            "Test module already exists: {}",
            full_path.display()
        ));
        return Ok(1);
    }

    if let Some(parent) = full_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
    }

    let content = generate_test_content(name);

    fs::write(&full_path, content)
        .with_context(|| format!("Failed to write file: {}", full_path.display()))?;

    output.status("Created", &format!("test/{}", relative_path.display()));

    Ok(0)
}

fn create_benchmark_module(name: &str, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    let relative_path = module_name_to_path(name);
    let full_path = project_root.join("bench").join(&relative_path);

    if full_path.exists() {
        output.error(&format!(
            "Benchmark module already exists: {}",
            full_path.display()
        ));
        return Ok(1);
    }

    if let Some(parent) = full_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
    }

    let content = generate_benchmark_content(name);

    fs::write(&full_path, content)
        .with_context(|| format!("Failed to write file: {}", full_path.display()))?;

    output.status("Created", &format!("bench/{}", relative_path.display()));

    Ok(0)
}

fn module_name_to_path(name: &str) -> PathBuf {
    let path_str = name.replace('.', "/");
    PathBuf::from(format!("{}.hs", path_str))
}

fn generate_module_content(name: &str) -> String {
    format!(
        r#"-- | Module: {name}
--
-- Description: TODO: Add module description
module {name}
  (
  ) where

"#,
        name = name
    )
}

fn generate_test_content(name: &str) -> String {
    format!(
        r#"-- | Tests for {name}
module {name} (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "{name}"
  [ testCase "example test" $ do
      1 + 1 @?= (2 :: Int)
  ]
"#,
        name = name
    )
}

fn generate_benchmark_content(name: &str) -> String {
    format!(
        r#"-- | Benchmarks for {name}
module {name} (benchmarks) where

import Criterion.Main

benchmarks :: Benchmark
benchmarks = bgroup "{name}"
  [ bench "example" $ nf (+ 1) (1 :: Int)
  ]
"#,
        name = name
    )
}

/// Create a project from a git repository template.
async fn create_project_from_git_template(
    url: &str,
    name: &str,
    dir: Option<String>,
    branch: &str,
    output: &Output,
) -> Result<i32> {
    let target_dir = dir.unwrap_or_else(|| name.to_string());
    let target_path = Path::new(&target_dir);

    // Check if directory already exists and is not empty
    if target_path.exists() {
        let entries: Vec<_> = fs::read_dir(target_path)?.collect();
        if !entries.is_empty() {
            output.error(&format!(
                "Directory '{}' already exists and is not empty",
                target_dir
            ));
            return Ok(1);
        }
    }

    // Normalize the URL (support GitHub shorthand)
    let git_url = normalize_git_url(url);

    output.status("Cloning", &format!("template from {}", git_url));

    // Clone the repository
    let spinner = Spinner::new("Downloading template...");

    let runner = CommandRunner::new();
    let clone_output = runner
        .run(
            "git",
            [
                "clone",
                "--depth=1",
                "--branch",
                branch,
                &git_url,
                &target_dir,
            ],
        )
        .await;

    match clone_output {
        Ok(result) if result.success() => {
            spinner.finish_success("Template downloaded");
        }
        Ok(result) => {
            spinner.finish_error("Clone failed");
            eprintln!("{}", result.stderr);

            // Try without branch specification
            output.info("Retrying without branch specification...");
            let retry = runner
                .run("git", ["clone", "--depth=1", &git_url, &target_dir])
                .await?;

            if !retry.success() {
                output.error("Failed to clone template repository");
                eprintln!("{}", retry.stderr);
                return Ok(1);
            }
        }
        Err(e) => {
            spinner.finish_error("Clone failed");
            output.error(&format!("Git error: {}", e));
            return Ok(1);
        }
    }

    // Remove .git directory to start fresh
    let git_dir = target_path.join(".git");
    if git_dir.exists() {
        fs::remove_dir_all(&git_dir)
            .with_context(|| "Failed to remove .git directory")?;
    }

    // Apply template variable substitution
    let spinner = Spinner::new("Applying template variables...");

    let ctx = TemplateContext::new(name);
    apply_template_substitutions(target_path, &ctx)?;

    // Rename files that contain template variables in their names
    rename_template_files(target_path, &ctx)?;

    spinner.finish_success("Template customized");

    // Initialize new git repository
    let _ = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(target_path)
        .status();

    output.status("Created", &format!("project '{}' in '{}'", name, target_dir));
    output.info("");
    output.info("Next steps:");
    output.info(&format!("  cd {}", target_dir));
    output.info("  hx build");

    Ok(0)
}

/// Normalize a git URL, supporting GitHub shorthand (user/repo).
fn normalize_git_url(url: &str) -> String {
    // If it's already a full URL, return as-is
    if url.starts_with("http://")
        || url.starts_with("https://")
        || url.starts_with("git@")
        || url.starts_with("ssh://")
    {
        return url.to_string();
    }

    // Check for GitHub shorthand (user/repo)
    if url.contains('/') && !url.contains(':') {
        return format!("https://github.com/{}.git", url);
    }

    url.to_string()
}

/// Apply template variable substitutions to all files in a directory.
fn apply_template_substitutions(dir: &Path, ctx: &TemplateContext) -> Result<()> {
    let entries: Vec<_> = walkdir::WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .collect();

    for entry in entries {
        let path = entry.path();

        // Skip binary files
        if is_binary_file(path) {
            continue;
        }

        // Read and substitute
        if let Ok(content) = fs::read_to_string(path) {
            let substituted = ctx.substitute(&content);
            if substituted != content {
                fs::write(path, substituted)?;
            }
        }
    }

    Ok(())
}

/// Rename files that contain template variables in their names.
fn rename_template_files(dir: &Path, ctx: &TemplateContext) -> Result<()> {
    let entries: Vec<_> = walkdir::WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .collect();

    // Process in reverse order (deepest first) to handle nested renames
    for entry in entries.into_iter().rev() {
        let path = entry.path();
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        // Check if filename contains template variables
        if file_name.contains("{{") {
            let new_name = ctx.substitute(file_name);
            if new_name != file_name {
                let new_path = path.with_file_name(&new_name);
                fs::rename(path, &new_path)?;
            }
        }
    }

    Ok(())
}

/// Check if a file is likely binary.
fn is_binary_file(path: &Path) -> bool {
    let binary_extensions = [
        "png", "jpg", "jpeg", "gif", "ico", "bmp", "webp",
        "pdf", "zip", "tar", "gz", "bz2", "xz", "7z",
        "exe", "dll", "so", "dylib", "a", "o",
        "woff", "woff2", "ttf", "otf", "eot",
        "mp3", "mp4", "avi", "mov", "wav", "ogg",
        "sqlite", "db",
    ];

    path.extension()
        .and_then(|e| e.to_str())
        .map(|e| binary_extensions.contains(&e.to_lowercase().as_str()))
        .unwrap_or(false)
}
