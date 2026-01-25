# hx Distribution Channels

Tracking all places where hx is or could be available.

## Live

| Channel | Install Command | Link |
|---------|-----------------|------|
| GitHub Releases | `curl -fsSL https://hx.raskell.io/install.sh \| sh` | [releases](https://github.com/raskell-io/hx/releases) |
| Homebrew | `brew install raskell-io/tap/hx` | [homebrew-tap](https://github.com/raskell-io/homebrew-tap) |
| Nix Flake | `nix run github:raskell-io/hx` | [flake.nix](../flake.nix) |
| Scoop | `scoop bucket add raskell-io ...` | [scoop-bucket](https://github.com/raskell-io/scoop-bucket) |
| aqua | `aqua g -i raskell-io/hx` | [aqua-registry](https://github.com/aquaproj/aqua-registry) |
| pkgx | `pkgx hx` | [pantry](https://github.com/pkgxdev/pantry) |

## Pending (PRs submitted)

| Channel | Status | Link |
|---------|--------|------|
| Nixpkgs | PR open (maintainers want more adoption first) | https://github.com/NixOS/nixpkgs/pull/483671 |
| asdf | PR open | https://github.com/asdf-vm/asdf-plugins/pull/1140 |
| mise | Uses asdf plugin | (waiting on asdf PR) |
| WinGet | PR open (CLA signed) | https://github.com/microsoft/winget-pkgs/pull/333584 |

## Pending (manual action needed)

| Channel | Blocker | Files Ready |
|---------|---------|-------------|
| Cargo | Need to publish to crates.io | `cargo publish -p hx-cli` |
| AUR | Need Arch Linux to solve CAPTCHA for account creation | [aur-hx-bin](https://github.com/raskell-io/aur-hx-bin) |
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
