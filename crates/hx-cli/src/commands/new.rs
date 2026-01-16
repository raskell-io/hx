//! Create new modules and files from templates.

use anyhow::{Context, Result};
use hx_config::find_project_root;
use hx_ui::Output;
use std::fs;
use std::path::PathBuf;

use crate::cli::NewCommands;

/// Run the new command.
pub async fn run(command: NewCommands, output: &Output) -> Result<i32> {
    match command {
        NewCommands::Module { name, src: _, test } => {
            let dir = if test { "test" } else { "src" };
            create_module(&name, dir, output)
        }
        NewCommands::Test { name } => create_test_module(&name, output),
        NewCommands::Benchmark { name } => create_benchmark_module(&name, output),
    }
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
