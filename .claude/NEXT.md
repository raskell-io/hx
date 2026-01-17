# What's Next: Path to cargo/uv Parity

## Current State

v0.1-v0.3 complete. hx has:
- Full CLI with build/test/run/lock/sync
- Toolchain management with direct downloads (no ghcup required)
- Native build support (experimental)
- Plugin system, watch mode, coverage, distribution
- Comprehensive documentation

## The Gap: hx is Still a Wrapper

The fundamental issue: hx orchestrates Cabal, it doesn't replace it.
- uv succeeded by rewriting pip in Rust
- Cargo **is** the build system
- hx needs to become the build system, not just wrap one

## What Makes cargo/uv Best-in-Class

| Trait | cargo | uv | hx today |
|-------|-------|-----|----------|
| **Speed** | Fast incremental | 10-100x pip | Cabal speed (slow) |
| **Zero config** | Just works | Just works | Needs hx.toml |
| **Error UX** | Exceptional | Exceptional | Good |
| **Ecosystem** | crates.io native | PyPI native | Hackage wrapper |
| **Add deps** | `cargo add` | `uv add` | `hx add` ✅ |
| **Offline** | Cached builds | Cached | Partial |
| **Hermetic** | Via lockfile | Via lockfile | Partial |
| **Cross-compile** | Built-in | N/A | Missing |

---

## Proposed Roadmap

### v0.5.0 - "Just Works" Release

**Goal**: First-run experience as smooth as cargo

```bash
# This should work on a fresh machine with just hx installed
curl -sSL https://hx.dev/install.sh | sh
hx new hello && cd hello && hx run
```

| Feature | Priority | Effort | Status |
|---------|----------|--------|--------|
| `hx add <pkg>` / `hx remove <pkg>` | P0 | Medium | ✅ |
| Auto-detect and install GHC on first run | P0 | Done | ✅ |
| `hx upgrade` self-update | P0 | Low | ✅ |
| Shell completions auto-install | P1 | Low | ✅ |
| `hx init` without flags (smart defaults) | P1 | Low | ✅ |
| Global config (`~/.hx/config.toml`) | P1 | Medium | ✅ |

### v0.6.0 - Speed Parity

**Goal**: Builds as fast or faster than plain GHC

| Feature | Impact | Effort | Status |
|---------|--------|--------|--------|
| Production-ready native builds | High | High | In Progress |
| Persistent compilation server | High | High | TODO |
| Parallel module compilation | High | Medium | Partial |
| Content-addressed build cache | High | Done | ✅ |
| Remote cache fetch (like Turborepo) | Medium | High | TODO |

### v0.7.0 - Ecosystem Integration

**Goal**: First-class Hackage citizen

| Feature | Impact | Effort | Status |
|---------|--------|--------|--------|
| `hx publish` with 2FA/token auth | High | Medium | Basic ✅ |
| `hx info <pkg>` - package details | Medium | Low | ✅ |
| `hx outdated` - check for updates | Medium | Low | ✅ |
| `hx why <pkg>` - why is this dep here? | Medium | Low | ✅ |
| `hx update` - update dependencies | Medium | Low | ✅ |
| `hx tree` / `hx list` - dependency views | Low | Low | ✅ |
| Revision support in lockfile | Medium | Medium | TODO |
| Stackage snapshot support | Medium | Medium | TODO |

### v0.8.0 - Advanced Builds

**Goal**: Handle real-world complexity

| Feature | Impact | Effort | Status |
|---------|--------|--------|--------|
| Custom Setup.hs support (native) | High | High | TODO |
| Preprocessor integration (alex/happy) | High | Medium | TODO |
| C/C++ source compilation | High | Medium | TODO |
| `hx build --target aarch64-linux` | Medium | High | TODO |
| Feature flags (like Cabal flags) | Medium | Medium | TODO |
| Build profiles (dev/release/custom) | Low | Low | TODO |

### v0.9.0 - Enterprise Ready

**Goal**: CI/CD and team workflows

| Feature | Impact | Effort | Status |
|---------|--------|--------|--------|
| Remote build cache (S3/GCS/HTTP) | High | High | TODO |
| `hx ci` optimized for CI | High | Medium | TODO |
| Dependency vendoring | Medium | Medium | TODO |
| SBOM generation | Medium | Low | TODO |
| Reproducible builds (byte-for-byte) | Medium | High | TODO |
| Multi-repo workspaces | Low | High | TODO |

### v1.0.0 - Cabal Independence

**Goal**: Full builds without Cabal installed

| Feature | Impact | Effort | Status |
|---------|--------|--------|--------|
| All Cabal build-types native | Critical | Very High | TODO |
| Package DB management native | Critical | High | Partial |
| Solver native (replace cabal-install) | Critical | Very High | Partial |
| Backpack support | Medium | Very High | TODO |

---

## Quick Wins (High Impact, Low Effort) ✅ ALL COMPLETE

### 1. `hx add` / `hx remove` ✅

```bash
# Add dependency (edit hx.toml + .cabal + run lock)
hx add aeson
hx add text ">=2.0"
hx remove old-dep
```

### 2. `hx why <pkg>` ✅

```bash
hx why bytestring
# bytestring 0.12.1.0
#   └── aeson 2.2.1.0 (direct)
#   └── text 2.1 (direct)
```

### 3. `hx outdated` ✅

```bash
hx outdated
# aeson    2.2.1.0 → 2.2.2.0  (minor)
# text     2.1     → 2.1.1    (patch)
```

### 4. `hx info <pkg>` ✅

```bash
hx info aeson
# aeson 2.2.2.0
# Fast JSON parsing and encoding
# License: BSD-3-Clause
# Maintainer: oleg@eyesopen.com
# Homepage: https://github.com/haskell/aeson
```

### 5. `hx update` ✅

```bash
hx update                # Update all (minor/patch only)
hx update --major        # Allow major updates
hx update aeson text     # Update specific packages
hx update --dry-run      # Preview changes
```

---

## The "10x Better" Features

### 1. Instant Feedback (like esbuild/swc)

```bash
hx check  # Type-check in <1s for incremental changes
```

**How**:
- Persistent GHC process (like ghcid)
- Or GHC's `-fwrite-if-simplified-core`
- Module-level caching

### 2. Smart Rebuilds (like Turborepo)

```bash
hx build  # "3 modules affected, skipping 47"
```

**How**:
- Fine-grained dependency tracking
- Content-hash based invalidation
- Only recompile truly affected modules

### 3. Shared Team Cache

```bash
hx build --cache=s3://company-cache/hx
# "Downloaded 23 cached artifacts, building 2 locally"
```

**How**:
- Content-addressed artifacts
- HTTP/S3/GCS backends
- Authentication integration

### 4. One-Command Setup

```bash
git clone <repo> && cd <repo> && hx build
# Auto-installs correct GHC, fetches deps, builds
```

**Status**: ✅ Already works with auto-install!

---

## Priority Order

1. ~~**`hx add/remove`** - Biggest UX gap vs cargo~~ ✅ DONE
2. **Native build production-ready** - Biggest speed opportunity
3. **Remote build cache** - Biggest CI/team value
4. **Cross-compilation** - Unique differentiator

---

## Metrics for Success

| Metric | Current | Target |
|--------|---------|--------|
| Fresh build (medium project) | ~60s | <30s |
| Incremental build (1 file) | ~5s | <1s |
| `hx add <pkg>` | ✅ ~1s | <2s |
| First-run setup | ~5min | <1min |
| CI build (cached) | ~3min | <30s |

---

## Reference: cargo/uv Feature Lists

### cargo features we should match
- `cargo add/remove` ✅ `hx add/rm`
- `cargo update` ✅ `hx update`
- `cargo tree` ✅ `hx tree`
- `cargo vendor`
- `cargo fetch` ✅ `hx fetch`
- `cargo package`
- `cargo login/logout`
- `cargo owner`
- `cargo yank`
- `cargo search` ✅ `hx search`
- `cargo install` (global binaries)
- `cargo uninstall`
- Build scripts (build.rs equivalent)
- Feature flags
- Target specification
- Profile customization

### uv features we should match
- Lightning fast resolution
- Lock file with hashes ✅ `hx lock`
- `uv add/remove` ✅ `hx add/rm`
- `uv sync` ✅ `hx sync`
- `uv run` (isolated execution)
- `uv tool` (global tools)
- Python version management ✅ `hx toolchain`
- Cache sharing
- Offline mode
