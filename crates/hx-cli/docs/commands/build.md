# hx build

Build the project.

## Usage

```bash
hx build [OPTIONS]
```

## Options

| Option | Short | Description |
|--------|-------|-------------|
| `--release` | `-r` | Build with optimizations |
| `--jobs <N>` | `-j` | Number of parallel jobs |
| `--target <TARGET>` | | Build specific target |
| `--package <PKG>` | `-p` | Build specific package (workspace) |
| `--native` | | Use native GHC builder (experimental) |
| `--no-auto-install` | | Don't auto-install toolchain |
| `--verbose` | `-v` | Verbose output |

## Examples

### Basic Build

```bash
hx build
```

### Release Build

```bash
hx build --release
```

### Parallel Build

```bash
hx build --jobs 8
```

### Workspace Package

```bash
hx build --package my-lib
```

### Native Build (Experimental)

```bash
hx build --native
```

## Behavior

1. **Find project** - Locate `hx.toml` in current or parent directory
2. **Check toolchain** - Verify GHC/Cabal versions, auto-install if needed
3. **Run pre-build hooks** - Execute configured scripts
4. **Check freshness** - Skip if nothing changed
5. **Build** - Run `cabal build` with configured options
6. **Run post-build hooks** - Execute configured scripts
7. **Report** - Show build status and timing

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Build succeeded |
| 4 | Toolchain error |
| 5 | Build failed |
| 6 | Hook failed |

## Output

### Normal

```
   Building my-project
    Finished in 2.3s
```

### Verbose

```
   Building my-project
   Compiling Main (1/3)
   Compiling Lib (2/3)
   Compiling Utils (3/3)
   Linking my-project
    Finished 3 modules in 2.3s
```

### Fresh (No Changes)

```
     Fresh my-project
      info: No changes detected, skipping build
```

## Environment

| Variable | Effect |
|----------|--------|
| `HX_JOBS` | Default parallelism |
| `HX_RELEASE` | Default to release mode |

## See Also

- [hx test](./test.md) - Run tests
- [hx run](./run.md) - Run executable
- [hx clean](./clean.md) - Clean build artifacts
