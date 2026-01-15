//! Format command implementation.

use anyhow::Result;
use hx_config::find_project_root;
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};

/// Run the fmt command.
pub async fn run(check: bool, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    output.status("Formatting", &project_root.display().to_string());

    // Try fourmolu first, then ormolu
    let (formatter, found) = detect_formatter().await;

    if !found {
        output.error("No formatter found (fourmolu or ormolu)");
        output.info("Install fourmolu: cabal install fourmolu");
        output.info("Or install ormolu: cabal install ormolu");
        return Ok(4);
    }

    let spinner = Spinner::new(format!("Running {}...", formatter));

    let runner = CommandRunner::new().with_working_dir(&project_root);

    // Find all .hs files
    let hs_files = find_haskell_files(&project_root)?;

    if hs_files.is_empty() {
        spinner.finish_warning("No Haskell files found");
        return Ok(0);
    }

    let mut args: Vec<String> = if check {
        vec!["--mode".to_string(), "check".to_string()]
    } else {
        vec!["--mode".to_string(), "inplace".to_string()]
    };

    // Add files
    for file in &hs_files {
        args.push(file.to_string_lossy().to_string());
    }

    let args_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let cmd_output = runner.run(formatter.as_str(), args_refs).await?;

    if cmd_output.success() {
        if check {
            spinner.finish_success("All files formatted correctly");
        } else {
            spinner.finish_success(format!("Formatted {} files", hs_files.len()));
        }
        Ok(0)
    } else {
        if check {
            spinner.finish_error("Some files need formatting");
            output.info("Run `hx fmt` to format files");
        } else {
            spinner.finish_error("Formatting failed");
        }
        if output.is_verbose() {
            eprintln!("{}", cmd_output.stderr);
        }
        Ok(1)
    }
}

async fn detect_formatter() -> (String, bool) {
    // Try fourmolu first
    if which::which("fourmolu").is_ok() {
        return ("fourmolu".to_string(), true);
    }

    // Try ormolu
    if which::which("ormolu").is_ok() {
        return ("ormolu".to_string(), true);
    }

    ("fourmolu".to_string(), false)
}

fn find_haskell_files(dir: &std::path::Path) -> Result<Vec<std::path::PathBuf>> {
    let mut files = Vec::new();

    fn walk(dir: &std::path::Path, files: &mut Vec<std::path::PathBuf>) -> std::io::Result<()> {
        if dir.is_dir() {
            // Skip hidden directories and common non-source directories
            let dir_name = dir.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if dir_name.starts_with('.') || dir_name == "dist-newstyle" {
                return Ok(());
            }

            for entry in std::fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    walk(&path, files)?;
                } else if path.extension().is_some_and(|ext| ext == "hs") {
                    files.push(path);
                }
            }
        }
        Ok(())
    }

    walk(dir, &mut files)?;
    Ok(files)
}
