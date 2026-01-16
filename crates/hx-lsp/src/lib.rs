//! Language Server Protocol implementation for hx.
//!
//! This crate provides a minimal LSP server for Haskell projects that:
//! - Publishes diagnostics from GHC type checking
//! - Provides quick-fix code actions
//! - Watches files for changes and re-checks on save

mod server;
mod state;
mod watcher;

pub use server::HxLanguageServer;
pub use state::{ServerState, WorkspaceState};

use anyhow::Result;
use tokio::net::TcpListener;
use tower_lsp::{LspService, Server};
use tracing::info;

/// Run the LSP server on stdio (default mode for editors).
pub async fn run_server() -> Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(HxLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}

/// Run the LSP server on a TCP port (useful for debugging).
pub async fn run_server_tcp(port: u16) -> Result<()> {
    let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
    info!("LSP server listening on port {}", port);

    loop {
        let (stream, addr) = listener.accept().await?;
        info!("Client connected from {}", addr);

        let (read, write) = tokio::io::split(stream);
        let (service, socket) = LspService::new(HxLanguageServer::new);
        Server::new(read, write, socket).serve(service).await;

        info!("Client disconnected");
    }
}
