# LSP Protocol Implementation

`hx-lsp` implements a subset of the Language Server Protocol focused on fast diagnostic feedback.

## Supported Methods

### Lifecycle Methods

| Method | Direction | Description |
|--------|-----------|-------------|
| `initialize` | Client → Server | Initialize server, negotiate capabilities |
| `initialized` | Client → Server | Client ready notification |
| `shutdown` | Client → Server | Shutdown request |
| `exit` | Client → Server | Exit notification |

### Text Synchronization

| Method | Direction | Description |
|--------|-----------|-------------|
| `textDocument/didOpen` | Client → Server | Document opened |
| `textDocument/didChange` | Client → Server | Document content changed |
| `textDocument/didSave` | Client → Server | Document saved (triggers check) |
| `textDocument/didClose` | Client → Server | Document closed |

### Diagnostics

| Method | Direction | Description |
|--------|-----------|-------------|
| `textDocument/publishDiagnostics` | Server → Client | Send diagnostics |

### Code Actions (Future)

| Method | Direction | Description |
|--------|-----------|-------------|
| `textDocument/codeAction` | Client → Server | Request quick fixes |

## Server Capabilities

The server advertises these capabilities during initialization:

```json
{
  "capabilities": {
    "textDocumentSync": {
      "openClose": true,
      "change": 1,
      "save": { "includeText": false }
    },
    "diagnosticProvider": {
      "interFileDependencies": true,
      "workspaceDiagnostics": false
    }
  }
}
```

## Diagnostic Severity Mapping

GHC diagnostics map to LSP severities:

| GHC | LSP Severity | Display |
|-----|--------------|---------|
| Error | 1 (Error) | Red squiggle |
| Warning | 2 (Warning) | Yellow squiggle |
| Hint | 4 (Hint) | Faded text |

## Message Flow

### Document Open

```
Client                          Server
  │                               │
  │─── textDocument/didOpen ─────►│
  │                               │
  │                    ┌──────────┤
  │                    │ Add to   │
  │                    │ tracked  │
  │                    │ documents│
  │                    └──────────┤
  │                               │
  │                    ┌──────────┤
  │                    │ Run GHC  │
  │                    │ check    │
  │                    └──────────┤
  │                               │
  │◄── publishDiagnostics ────────│
  │                               │
```

### Document Save

```
Client                          Server
  │                               │
  │─── textDocument/didSave ─────►│
  │                               │
  │                    ┌──────────┤
  │                    │ Debounce │
  │                    │ 500ms    │
  │                    └──────────┤
  │                               │
  │                    ┌──────────┤
  │                    │ Run GHC  │
  │                    │ check    │
  │                    └──────────┤
  │                               │
  │◄── publishDiagnostics ────────│
  │                               │
```

## Diagnostic Format

### From GHC

```json
{
  "span": {
    "file": "src/Main.hs",
    "startLine": 10,
    "startCol": 5,
    "endLine": 10,
    "endCol": 15
  },
  "severity": "Error",
  "message": "Variable not in scope: foo"
}
```

### To LSP

```json
{
  "uri": "file:///project/src/Main.hs",
  "diagnostics": [
    {
      "range": {
        "start": { "line": 9, "character": 4 },
        "end": { "line": 9, "character": 14 }
      },
      "severity": 1,
      "source": "ghc",
      "message": "Variable not in scope: foo"
    }
  ]
}
```

Note: LSP uses 0-indexed lines/columns, GHC uses 1-indexed.

## Transport Modes

### stdio (Default)

Standard mode for editor integration:

```rust
pub async fn run_server() -> Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(HxLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}
```

### TCP (Debug)

For debugging with external tools:

```rust
pub async fn run_server_tcp(port: u16) -> Result<()> {
    let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
    // Accept connections...
}
```

## Error Handling

### Document Not Found

```json
{
  "uri": "file:///project/src/Missing.hs",
  "diagnostics": []
}
```

Clear diagnostics for missing files.

### GHC Crash

```json
{
  "method": "window/showMessage",
  "params": {
    "type": 1,
    "message": "GHC check failed: process exited with code 1"
  }
}
```

### Invalid Request

Return LSP error response with appropriate code:

| Code | Meaning |
|------|---------|
| -32700 | Parse error |
| -32600 | Invalid request |
| -32601 | Method not found |
| -32602 | Invalid params |
| -32603 | Internal error |

## Dependencies

The LSP server is built with:

- `tower-lsp` - LSP protocol handling
- `tokio` - Async runtime
- `serde_json` - JSON serialization
