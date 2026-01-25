# hx Distribution Channels

Tracking all places where hx is or could be available.

## Live

| Channel | Install Command | Link |
|---------|-----------------|------|
| GitHub Releases | `curl -fsSL .../install.sh \| sh` | [releases](https://github.com/raskell-io/hx/releases) |
| Cargo | `cargo install hx-cli` | [crates.io](https://crates.io/crates/hx-cli) |
| Homebrew | `brew install raskell-io/tap/hx` | [homebrew-tap](https://github.com/raskell-io/homebrew-tap) |
| Nix Flake | `nix run github:raskell-io/hx` | [flake.nix](../flake.nix) |
| Scoop | `scoop bucket add raskell-io ...` | [scoop-bucket](https://github.com/raskell-io/scoop-bucket) |

## Pending (PRs submitted)

| Channel | Status | Link |
|---------|--------|------|
| asdf | PR open | https://github.com/asdf-vm/asdf-plugins/pull/1140 |
| aqua | PR open | https://github.com/aquaproj/aqua-registry/pull/47579 |
| mise | Uses asdf plugin | (waiting on asdf PR) |
| WinGet | PR open | https://github.com/microsoft/winget-pkgs/pull/333584 |
| pkgx | PR open | https://github.com/pkgxdev/pantry/pull/11657 |

## Pending (manual action needed)

| Channel | Blocker | Files Ready |
|---------|---------|-------------|
| AUR | Need Arch Linux to solve CAPTCHA for account creation | [aur-hx-bin](https://github.com/raskell-io/aur-hx-bin) |
| Nixpkgs | Need to compute cargoHash with nix, submit PR | [issue](https://github.com/NixOS/nixpkgs/issues/483568), [package.nix](../contrib/nix/package.nix) |
| FlakeHub | Publishes on next tag release | [workflow](../.github/workflows/flakehub.yml) |
| Chocolatey | Need Windows + Chocolatey account to publish | [chocolatey-hx](https://github.com/raskell-io/chocolatey-hx) |
| Docker/GHCR | Publishes on next tag release | [workflow](../.github/workflows/docker.yml) |

## Plugin/Package repos

| Repo | Purpose |
|------|---------|
| [raskell-io/asdf-hx](https://github.com/raskell-io/asdf-hx) | asdf plugin |
| [raskell-io/mise-hx](https://github.com/raskell-io/mise-hx) | mise plugin |
| [raskell-io/homebrew-tap](https://github.com/raskell-io/homebrew-tap) | Homebrew formula |
| [raskell-io/scoop-bucket](https://github.com/raskell-io/scoop-bucket) | Scoop bucket |
| [raskell-io/aur-hx-bin](https://github.com/raskell-io/aur-hx-bin) | AUR PKGBUILD |
| [raskell-io/chocolatey-hx](https://github.com/raskell-io/chocolatey-hx) | Chocolatey package |
