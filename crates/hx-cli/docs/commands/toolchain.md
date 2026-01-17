# hx toolchain

Manage Haskell toolchain versions.

## Usage

```bash
hx toolchain <COMMAND>
```

## Subcommands

| Command | Description |
|---------|-------------|
| `install` | Install GHC/Cabal versions |
| `list` | List installed versions |
| `use` | Set active version |
| `remove` | Remove installed version |
| `status` | Show current toolchain |

## hx toolchain install

Install toolchain components.

```bash
hx toolchain install [OPTIONS]

Options:
    --ghc <VERSION>     Install specific GHC version
    --cabal <VERSION>   Install specific Cabal version
    --set              Set as active after install
```

### Examples

```bash
# Install from hx.toml requirements
hx toolchain install

# Install specific GHC
hx toolchain install --ghc 9.8.2

# Install both
hx toolchain install --ghc 9.8.2 --cabal 3.12.1.0
```

## hx toolchain list

List installed versions.

```bash
hx toolchain list
```

Output:
```
GHC versions:
  * 9.8.2 (active)
    9.6.4
    9.4.8

Cabal versions:
  * 3.12.1.0 (active)
    3.10.3.0
```

## hx toolchain use

Set active toolchain version.

```bash
hx toolchain use --ghc <VERSION>
hx toolchain use --cabal <VERSION>
```

### Examples

```bash
# Switch GHC version
hx toolchain use --ghc 9.6.4

# Switch Cabal version
hx toolchain use --cabal 3.10.3.0
```

## hx toolchain remove

Remove an installed version.

```bash
hx toolchain remove --ghc <VERSION>
hx toolchain remove --cabal <VERSION>
```

### Examples

```bash
# Remove old GHC
hx toolchain remove --ghc 9.4.8
```

## hx toolchain status

Show current toolchain status.

```bash
hx toolchain status
```

Output:
```
Toolchain Status
================

GHC:
  Version: 9.8.2
  Path: /Users/user/.hx/toolchains/ghc/9.8.2/bin/ghc
  Source: hx-managed

Cabal:
  Version: 3.12.1.0
  Path: /Users/user/.hx/toolchains/cabal/3.12.1.0/bin/cabal
  Source: hx-managed

Project Requirements (from hx.toml):
  GHC: 9.8.2 ✓
  Cabal: 3.12.1.0 ✓
```

## Installation Sources

hx can install toolchains from:

1. **Direct download** - From downloads.haskell.org (no ghcup required)
2. **GHCup** - Via ghcup if available

The smart installer tries direct download first, then falls back to ghcup.

## Installation Directory

Toolchains are installed to:

```
~/.hx/toolchains/
├── ghc/
│   ├── 9.8.2/
│   └── 9.6.4/
├── cabal/
│   └── 3.12.1.0/
└── manifest.json
```

## Auto-Installation

When `hx.toml` specifies toolchain versions, `hx build` will automatically install them:

```toml
[toolchain]
ghc = "9.8.2"
cabal = "3.12.1.0"
```

```bash
$ hx build
   Downloading GHC 9.8.2...
   Downloading Cabal 3.12.1.0...
    Building my-project
```

Disable with `--no-auto-install`.

## See Also

- [hx doctor](./doctor.md) - Diagnose toolchain issues
- [Auto-Install](../../hx-toolchain/docs/auto-install.md) - Auto-installation details
