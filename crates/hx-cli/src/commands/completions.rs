//! Shell completion generation and installation.

use anyhow::Result;
use clap::CommandFactory;
use clap_complete::Shell;
use hx_ui::Output;
use std::env;
use std::fs;
use std::path::PathBuf;

use crate::cli::Cli;

/// Generate shell completions and print to stdout.
pub fn generate(shell: Shell) -> Result<i32> {
    let mut cmd = Cli::command();
    let name = cmd.get_name().to_string();

    clap_complete::generate(shell, &mut cmd, name, &mut std::io::stdout());

    Ok(0)
}

/// Install shell completions for the user's shell.
pub async fn install(shell: Option<Shell>, output: &Output) -> Result<i32> {
    // Detect shell if not specified
    let shell = match shell {
        Some(s) => s,
        None => detect_shell()?,
    };

    output.status("Installing", &format!("{} completions", shell_name(&shell)));

    // Generate completions to a string
    let completions = generate_completions(&shell);

    // Get the installation path
    let (install_path, source_instruction) = get_install_path(&shell)?;

    // Create parent directories if needed
    if let Some(parent) = install_path.parent() {
        fs::create_dir_all(parent)?;
    }

    // Write completions file
    fs::write(&install_path, &completions)?;

    output.info(&format!("  Wrote completions to {}", install_path.display()));

    // Check if sourcing is needed
    if let Some(instruction) = source_instruction {
        println!();
        output.info("To enable completions, add this to your shell config:");
        println!();
        println!("    {}", instruction);
        println!();
    } else {
        println!();
        output.info("Completions installed! Restart your shell or open a new terminal.");
    }

    Ok(0)
}

/// Detect the current shell from environment.
fn detect_shell() -> Result<Shell> {
    // Check $SHELL environment variable
    if let Ok(shell_path) = env::var("SHELL") {
        let shell_name = shell_path
            .rsplit('/')
            .next()
            .unwrap_or(&shell_path);

        match shell_name {
            "bash" => return Ok(Shell::Bash),
            "zsh" => return Ok(Shell::Zsh),
            "fish" => return Ok(Shell::Fish),
            "elvish" => return Ok(Shell::Elvish),
            "pwsh" | "powershell" => return Ok(Shell::PowerShell),
            _ => {}
        }
    }

    // Fallback: check if running in a specific shell
    if env::var("FISH_VERSION").is_ok() {
        return Ok(Shell::Fish);
    }
    if env::var("ZSH_VERSION").is_ok() {
        return Ok(Shell::Zsh);
    }
    if env::var("BASH_VERSION").is_ok() {
        return Ok(Shell::Bash);
    }

    // Default to bash
    Ok(Shell::Bash)
}

/// Get human-readable shell name.
fn shell_name(shell: &Shell) -> &'static str {
    match shell {
        Shell::Bash => "Bash",
        Shell::Zsh => "Zsh",
        Shell::Fish => "Fish",
        Shell::Elvish => "Elvish",
        Shell::PowerShell => "PowerShell",
        _ => "shell",
    }
}

/// Generate completions as a string.
fn generate_completions(shell: &Shell) -> Vec<u8> {
    let mut cmd = Cli::command();
    let name = cmd.get_name().to_string();
    let mut buf = Vec::new();

    clap_complete::generate(*shell, &mut cmd, name, &mut buf);

    buf
}

/// Get the installation path and optional source instruction for the shell.
fn get_install_path(shell: &Shell) -> Result<(PathBuf, Option<String>)> {
    let home = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("Could not find home directory"))?;

    match shell {
        Shell::Bash => {
            // Try XDG first, then fallback to ~/.local/share
            let data_dir = dirs::data_local_dir()
                .unwrap_or_else(|| home.join(".local/share"));
            let completions_dir = data_dir.join("bash-completion/completions");
            let path = completions_dir.join("hx");

            // Check if bash-completion is set up
            let bashrc = home.join(".bashrc");
            let source_instruction = if bashrc.exists() {
                let content = fs::read_to_string(&bashrc).unwrap_or_default();
                if content.contains("bash-completion") || content.contains(&completions_dir.display().to_string()) {
                    None
                } else {
                    Some(format!("source {}", path.display()))
                }
            } else {
                Some(format!("source {}", path.display()))
            };

            Ok((path, source_instruction))
        }
        Shell::Zsh => {
            // Check for common completion directories
            let fpath_dirs = [
                home.join(".zsh/completions"),
                home.join(".local/share/zsh/site-functions"),
                dirs::data_local_dir()
                    .unwrap_or_else(|| home.join(".local/share"))
                    .join("zsh/site-functions"),
            ];

            // Use the first one, create if needed
            let completions_dir = fpath_dirs[0].clone();
            let path = completions_dir.join("_hx");

            // Check if fpath includes this directory
            let zshrc = home.join(".zshrc");
            let zsh_instruction = "fpath=(~/.zsh/completions $fpath) && autoload -Uz compinit && compinit".to_string();
            let source_instruction = if zshrc.exists() {
                let content = fs::read_to_string(&zshrc).unwrap_or_default();
                if content.contains(&completions_dir.display().to_string()) {
                    None
                } else {
                    Some(zsh_instruction)
                }
            } else {
                Some(zsh_instruction)
            };

            Ok((path, source_instruction))
        }
        Shell::Fish => {
            // Fish has a standard completions directory
            let config_dir = dirs::config_dir()
                .unwrap_or_else(|| home.join(".config"));
            let path = config_dir.join("fish/completions/hx.fish");

            // Fish auto-loads from this directory
            Ok((path, None))
        }
        Shell::Elvish => {
            let config_dir = dirs::config_dir()
                .unwrap_or_else(|| home.join(".config"));
            let path = config_dir.join("elvish/lib/hx.elv");

            Ok((path, Some("use hx".to_string())))
        }
        Shell::PowerShell => {
            // PowerShell profile location varies
            let documents = dirs::document_dir()
                .unwrap_or_else(|| home.join("Documents"));
            let path = documents.join("PowerShell/Modules/hx/hx.psm1");

            Ok((path, Some("Import-Module hx".to_string())))
        }
        _ => {
            anyhow::bail!("Unsupported shell for auto-install")
        }
    }
}
