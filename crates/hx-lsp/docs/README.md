# hx-lsp

Minimal Language Server Protocol implementation for hx.

## Overview

`hx-lsp` provides a lightweight LSP server that:

- Publishes GHC diagnostics (errors, warnings)
- Watches for file changes
- Supports multiple documents
- Integrates with editors via stdio or TCP

## Quick Start

```rust
use hx_lsp::run_server;

// Run on stdin/stdout (editor mode)
run_server().await?;

// Or run on TCP (debugging)
use hx_lsp::run_server_tcp;
run_server_tcp(9999).await?;
```

## Architecture

```
┌─────────┐     stdio/TCP     ┌──────────┐
│ Editor  │◄─────────────────►│ hx-lsp   │
│ (VSCode,│    LSP protocol   │ server   │
│ Neovim) │                   │          │
└─────────┘                   └────┬─────┘
                                   │
                                   │ runs GHC
                                   ▼
                              ┌──────────┐
                              │   GHC    │
                              │ (via hx) │
                              └──────────┘
```

## Supported LSP Features

### Text Document Synchronization

- `textDocument/didOpen` - Track opened documents
- `textDocument/didChange` - Update document content
- `textDocument/didClose` - Stop tracking document

### Diagnostics

- `textDocument/publishDiagnostics` - Send errors/warnings

### Future Features

- [ ] `textDocument/completion` - Code completion
- [ ] `textDocument/hover` - Type information
- [ ] `textDocument/definition` - Go to definition
- [ ] `textDocument/codeAction` - Quick fixes

## File Watching

The server watches for file system changes:

```rust
use hx_lsp::watcher::{FileWatcher, FileChange, FileChangeKind};

let watcher = FileWatcher::new(&project_root, 500)?;  // 500ms debounce

loop {
    if let Some(changes) = watcher.try_recv() {
        for change in changes {
            match change.kind {
                FileChangeKind::Modified => {
                    // Re-check file
                }
                FileChangeKind::Removed => {
                    // Clear diagnostics
                }
            }
        }
    }
}
```

## Server State

```rust
pub struct ServerState {
    /// Open documents
    documents: HashMap<Url, DocumentState>,

    /// Project root
    root: PathBuf,

    /// GHC diagnostics
    diagnostics: HashMap<Url, Vec<Diagnostic>>,
}

pub struct DocumentState {
    /// Document content
    content: String,

    /// Document version
    version: i32,

    /// Last modification time
    modified_at: Instant,
}
```

## Running the Server

### From CLI

```bash
# Start LSP server on stdio
hx lsp

# Start on TCP port (for debugging)
hx lsp --tcp 9999
```

### Editor Configuration

#### VS Code

Install the Haskell extension - it will find hx-lsp automatically if `hx` is in PATH.

Or configure manually in `settings.json`:

```json
{
  "haskell.serverExecutablePath": "hx",
  "haskell.serverArgs": ["lsp"]
}
```

#### Neovim (nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

configs.hx_lsp = {
  default_config = {
    cmd = { 'hx', 'lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = lspconfig.util.root_pattern('hx.toml', 'cabal.project', '*.cabal'),
  },
}

lspconfig.hx_lsp.setup{}
```

#### Emacs (lsp-mode)

```elisp
(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection '("hx" "lsp"))
    :major-modes '(haskell-mode)
    :server-id 'hx-lsp))
```

## Diagnostic Flow

```
1. File saved
        │
        ▼
2. FileWatcher detects change
        │
        ▼
3. Server runs: hx check --json <file>
        │
        ▼
4. Parse GHC JSON diagnostics
        │
        ▼
5. Convert to LSP Diagnostic format
        │
        ▼
6. Send textDocument/publishDiagnostics
        │
        ▼
7. Editor displays errors/warnings
```

## Module Structure

| Module | Description |
|--------|-------------|
| `server` | LSP server implementation |
| `state` | Document and workspace state |
| `watcher` | File system monitoring |

## Error Handling

The LSP server handles errors gracefully:

```rust
// Invalid request → send error response
// File not found → clear diagnostics
// GHC crash → send notification
// Watch error → log and continue
```

## Logging

Enable debug logging:

```bash
RUST_LOG=hx_lsp=debug hx lsp
```

## Documentation

- [LSP Protocol](./protocol.md) - Protocol implementation details
- [Editor Setup](./editor-setup.md) - Configuration for VS Code, Neovim, Emacs, etc.
