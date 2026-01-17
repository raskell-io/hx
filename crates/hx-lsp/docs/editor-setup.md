# Editor Setup

Configure your editor to use the hx language server for Haskell development.

## VS Code

### Using Haskell Extension

The official Haskell extension can use hx-lsp:

1. Install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
2. Configure in `settings.json`:

```json
{
  "haskell.serverExecutablePath": "hx",
  "haskell.serverExtraArgs": ["lsp"]
}
```

### Manual Configuration

For a minimal setup without the full Haskell extension:

```json
{
  "languageServerExample.serverPath": "hx",
  "languageServerExample.serverArgs": ["lsp"]
}
```

## Neovim

### nvim-lspconfig

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define hx-lsp configuration
if not configs.hx_lsp then
  configs.hx_lsp = {
    default_config = {
      cmd = { 'hx', 'lsp' },
      filetypes = { 'haskell', 'lhaskell', 'cabal' },
      root_dir = lspconfig.util.root_pattern(
        'hx.toml',
        'cabal.project',
        '*.cabal',
        'stack.yaml'
      ),
      settings = {},
    },
  }
end

-- Enable the server
lspconfig.hx_lsp.setup {
  on_attach = function(client, bufnr)
    -- Your on_attach function
  end,
}
```

### lazy.nvim

```lua
{
  'neovim/nvim-lspconfig',
  config = function()
    local lspconfig = require('lspconfig')
    local configs = require('lspconfig.configs')

    configs.hx_lsp = {
      default_config = {
        cmd = { 'hx', 'lsp' },
        filetypes = { 'haskell', 'lhaskell' },
        root_dir = lspconfig.util.root_pattern('hx.toml', '*.cabal'),
      },
    }

    lspconfig.hx_lsp.setup {}
  end,
}
```

## Emacs

### lsp-mode

```elisp
(use-package lsp-mode
  :hook (haskell-mode . lsp)
  :config
  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-stdio-connection '("hx" "lsp"))
      :major-modes '(haskell-mode)
      :server-id 'hx-lsp
      :priority -1)))  ; Lower priority than hls
```

### eglot

```elisp
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("hx" "lsp"))))
```

## Helix

Add to `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "haskell"
language-server = { command = "hx", args = ["lsp"] }
roots = ["hx.toml", "*.cabal", "cabal.project"]
```

## Zed

Add to your Zed settings:

```json
{
  "languages": {
    "Haskell": {
      "language_server": {
        "command": "hx",
        "args": ["lsp"]
      }
    }
  }
}
```

## Sublime Text

Using LSP package:

1. Install [LSP](https://packagecontrol.io/packages/LSP)
2. Create `~/.config/sublime-text/Packages/User/LSP.sublime-settings`:

```json
{
  "clients": {
    "hx-lsp": {
      "enabled": true,
      "command": ["hx", "lsp"],
      "selector": "source.haskell"
    }
  }
}
```

## Debugging

### TCP Mode

For debugging the language server:

```bash
# Start server on port 9999
hx lsp --tcp 9999
```

Then configure your editor to connect to `127.0.0.1:9999`.

### Logging

Enable debug logging:

```bash
RUST_LOG=hx_lsp=debug hx lsp
```

View logs in stderr or redirect to file:

```bash
RUST_LOG=hx_lsp=debug hx lsp 2>/tmp/hx-lsp.log
```

### VS Code Output

Check the "Output" panel → "Haskell" for server logs.

### Neovim

```vim
:LspLog
```

Or check `~/.local/state/nvim/lsp.log`.

## Troubleshooting

### Server Not Starting

1. Verify hx is in PATH: `which hx`
2. Check server works: `hx lsp` (Ctrl+C to exit)
3. Check permissions on project directory

### No Diagnostics

1. Ensure project has `hx.toml` or `.cabal` file
2. Check GHC is installed: `hx doctor`
3. Verify file is saved (diagnostics on save)

### Slow Diagnostics

1. Use `hie.yaml` for multi-package projects: `hx ide setup`
2. Check for large dependency trees
3. Consider using `hx build --watch` for faster feedback

### Conflicts with HLS

If using both hx-lsp and HLS:

1. Disable one in your editor config
2. Or set priority to prefer one over the other

For lsp-mode:
```elisp
:priority -1  ; Lower priority = fallback
```

For nvim-lspconfig:
```lua
-- Only attach if no other Haskell LSP
on_attach = function(client, bufnr)
  local clients = vim.lsp.get_clients({ bufnr = bufnr })
  for _, c in ipairs(clients) do
    if c.name == 'hls' then
      client.stop()
      return
    end
  end
end
```
