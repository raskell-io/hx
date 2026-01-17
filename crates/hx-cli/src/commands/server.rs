//! Server command implementation.

use anyhow::Result;
use hx_cabal::server::{CompilationServer, ServerConfig, is_server_running, server_socket_path};
use hx_cabal::native::GhcConfig;
use hx_config::{Project, find_project_root};
use hx_ui::Output;
use std::path::PathBuf;

/// Run the server start command.
pub async fn start(output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Check if server is already running
    if is_server_running(&project.root) {
        output.warn("Compilation server is already running");
        output.info(&format!(
            "Socket: {}",
            server_socket_path(&project.root).display()
        ));
        return Ok(0);
    }

    output.status("Starting", "compilation server");

    // Detect GHC
    let ghc_config = match GhcConfig::detect().await {
        Ok(config) => config,
        Err(e) => {
            output.error(&format!("Failed to detect GHC: {}", e));
            return Ok(4);
        }
    };

    // Get source directories from config
    let src_dirs: Vec<PathBuf> = project
        .manifest
        .build
        .src_dirs
        .iter()
        .map(|s| project.root.join(s))
        .collect();

    // Build server config
    let config = ServerConfig {
        src_dirs,
        package_dbs: ghc_config.package_dbs.clone(),
        packages: ghc_config.packages.clone(),
        ghc_path: ghc_config.ghc_path.parent().unwrap().join("ghci"),
        idle_timeout_ms: 0,
        extensions: project.manifest.build.ghc_flags.clone(),
        ghci_options: Vec::new(),
    };

    // Start the server
    match CompilationServer::start(project.root.clone(), config).await {
        Ok(server) => {
            output.status("Started", "compilation server");

            // Get status
            if let Ok(status) = server.status().await {
                if let Some(ref version) = status.ghc_version {
                    output.info(version);
                }
            }

            output.info(&format!(
                "Socket: {}",
                server_socket_path(&project.root).display()
            ));

            // Keep server running
            output.info("Server running. Press Ctrl+C to stop.");

            // Wait for interrupt
            tokio::signal::ctrl_c().await?;

            output.status("Stopping", "compilation server");
            server.shutdown().await?;

            Ok(0)
        }
        Err(e) => {
            output.error(&format!("Failed to start server: {}", e));
            Ok(1)
        }
    }
}

/// Run the server stop command.
pub async fn stop(output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    let socket_path = server_socket_path(&project_root);
    if !socket_path.exists() {
        output.info("No compilation server running");
        return Ok(0);
    }

    output.status("Stopping", "compilation server");

    // Remove socket file
    if let Err(e) = std::fs::remove_file(&socket_path) {
        output.warn(&format!("Failed to remove socket file: {}", e));
    }

    output.status("Stopped", "compilation server");
    Ok(0)
}

/// Run the server status command.
pub async fn status(output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    let socket_path = server_socket_path(&project_root);
    if !socket_path.exists() {
        output.info("Compilation server: not running");
        return Ok(0);
    }

    output.info("Compilation server: running");
    output.info(&format!("Socket: {}", socket_path.display()));

    Ok(0)
}

/// Run the server restart command.
pub async fn restart(output: &Output) -> Result<i32> {
    // Stop existing server
    stop(output).await?;

    // Start new server
    start(output).await
}
