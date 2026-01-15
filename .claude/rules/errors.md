# Error UX Guidelines

## The "Astral Effect"

Our error messages should be so good that users know exactly what went wrong and how to fix it. This is our key differentiator.

## Failure Classes

Structure errors into these categories:

| Class | Description | Example |
|-------|-------------|---------|
| `ToolchainMissing` | Required tool not found | ghc, cabal, ghcup not in PATH |
| `ToolchainMismatch` | Wrong version installed | Expected GHC 9.8.2, found 9.6.x |
| `HlsMismatch` | HLS doesn't match GHC | HLS 2.8 with GHC 9.8 |
| `SystemDepMissing` | Native library not found | gmp, zlib, ncurses |
| `SolverFailure` | Package resolution failed | Conflicting constraints |
| `BuildFailure` | Compilation error | Type errors, parse errors |
| `ConfigError` | Invalid configuration | Malformed hx.toml |

## Error Output Format

Every error should have:

1. **Short summary** - One line describing what failed
2. **Context** - What operation was attempted
3. **Fix command(s)** - Direct actionable fix
4. **Hint** - Additional guidance if needed

```
error: GHC version mismatch
  expected: 9.8.2 (from hx.toml)
  found: 9.6.4

fix: Run `hx toolchain install --ghc 9.8.2`
```

## Implementation Pattern

```rust
pub struct Diagnostic {
    pub severity: Severity,
    pub code: ErrorCode,
    pub summary: String,
    pub context: Option<String>,
    pub fixes: Vec<Fix>,
    pub hints: Vec<String>,
}

pub struct Fix {
    pub description: String,
    pub command: Option<String>,
}
```

## Rules

1. **Never show raw stderr** - Parse and present structured output
2. **Always suggest a fix** - Even if it's "run `hx doctor`"
3. **No stack traces in normal output** - Use `--verbose` for that
4. **Use colors sparingly** - Red for errors, yellow for warnings
5. **Respect NO_COLOR** - Check env var before colorizing

## Common Error Patterns

### Missing Tool
```
error: ghcup not found
  GHCup is required to manage GHC installations.

fix: Install ghcup with:
     curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Build Failure
```
error: build failed with 3 errors

  src/Main.hs:15:10: error:
    Variable not in scope: undefined

  src/Lib.hs:23:5: error:
    Couldn't match expected type 'Int' with actual type 'String'

hint: Run `hx build --verbose` for full compiler output
```

### Lock Mismatch
```
error: hx.lock is out of sync with hx.toml
  Toolchain version changed: ghc 9.6.4 -> 9.8.2

fix: Run `hx lock` to update the lockfile
     or `hx sync --force` to override
```
