//! Language server command.

use anyhow::Result;
use hx_ui::Output;
use tracing::info;

/// Run the language server.
pub async fn run(tcp_port: Option<u16>, output: &Output) -> Result<i32> {
    if let Some(port) = tcp_port {
        output.status("Starting", &format!("LSP server on TCP port {}", port));
        info!("Starting LSP server on TCP port {}", port);
        hx_lsp::run_server_tcp(port).await?;
    } else {
        // stdio mode - don't print anything as it would interfere with the protocol
        info!("Starting LSP server on stdio");
        hx_lsp::run_server().await?;
    }

    Ok(0)
}
