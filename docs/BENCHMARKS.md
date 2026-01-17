# hx Build Performance Benchmarks

This document tracks build performance comparisons between native builds and cabal-based builds.

## Native vs Cabal Build Performance

**Test environment:**
- Date: 2026-01-17
- hx version: 0.3.6
- GHC version: 9.8.2
- Platform: macOS (Apple Silicon)

**Test project:** Simple 3-module executable
- Main.hs, Lib.hs, Utils.hs
- Single dependency: base

### Results

| Build Type | Native | Cabal | Speedup |
|------------|--------|-------|---------|
| Cold build (clean) | 0.48s | 2.68s | **5.6x** |
| Incremental (no changes) | 0.05s | 0.39s | **7.8x** |

### Notes

- **Cold build**: Fresh build after removing all build artifacts (`.hx/`, `dist-newstyle/`)
- **Incremental**: Subsequent build with no source changes
- Native build times include toolchain detection, module graph construction, and compilation
- Cabal build times include cabal's own dependency checking and build orchestration

## Preprocessor Performance

Preprocessors (alex, happy, hsc2hs) add minimal overhead:

| Preprocessor | File Count | Additional Time |
|--------------|------------|-----------------|
| alex (.x) | 1 | ~50ms |
| happy (.y) | 1 | ~100ms |
| hsc2hs (.hsc) | 1 | ~335ms |

## Running Benchmarks

To reproduce these benchmarks:

```bash
# Create test project
mkdir /tmp/hx-bench && cd /tmp/hx-bench
cat > hx.toml << 'EOF'
[project]
name = "hx-bench"
[toolchain]
ghc = "9.8.2"
EOF

cat > hx-bench.cabal << 'EOF'
cabal-version: 3.0
name: hx-bench
version: 0.1.0.0
build-type: Simple

executable hx-bench
    main-is: Main.hs
    other-modules: Lib, Utils
    hs-source-dirs: src
    default-language: GHC2021
    build-depends: base
EOF

mkdir src
echo 'module Main where; import Lib; import Utils; main = putStrLn (format greeting)' > src/Main.hs
echo 'module Lib (greeting) where; greeting = "Hello"' > src/Lib.hs
echo 'module Utils (format) where; format s = ">>> " ++ s ++ " <<<"' > src/Utils.hs

# Benchmark native build
rm -rf .hx dist-newstyle
time hx build --native

# Benchmark cabal build
rm -rf .hx dist-newstyle
time hx build
```

## Historical Results

| Version | Date | Native Cold | Cabal Cold | Speedup |
|---------|------|-------------|------------|---------|
| 0.3.6 | 2026-01-17 | 0.48s | 2.68s | 5.6x |
