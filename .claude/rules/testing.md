# Testing Guidelines

## Test Categories

### Unit Tests
Located in each crate's `src/` directory:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_config() {
        // ...
    }
}
```

Test these thoroughly:
- `hx.toml` parsing
- `hx.lock` read/write roundtrip
- Command argument parsing
- Path resolution
- Version parsing

### Integration Tests
Located in `tests/` directory of each crate:

```rust
// crates/hx-cli/tests/init.rs
#[test]
fn test_init_creates_project() {
    let temp = tempfile::tempdir().unwrap();
    // spawn hx init into temp dir
    // verify files created
}
```

Test these scenarios:
- `hx init` into temp directory
- `hx doctor` in empty directory
- Mock cabal command via PATH injection
- Lock generation from fixture outputs

### Golden Tests
For output stability:

```rust
#[test]
fn test_error_output_format() {
    let output = format_error(&error);
    insta::assert_snapshot!(output);
}
```

Use for:
- Error message formatting
- Cabal output parsing
- Diagnostic generation

## Test Fixtures

Store in `crates/<crate>/fixtures/`:

```
fixtures/
  hx.toml/
    minimal.toml
    full.toml
    invalid.toml
  cabal-output/
    build-success.txt
    build-failure.txt
    freeze.txt
```

## Mocking External Tools

Create mock binaries for testing:

```rust
fn with_mock_ghc<F: FnOnce()>(version: &str, f: F) {
    let mock_dir = create_mock_ghc(version);
    let original_path = env::var("PATH").unwrap();
    env::set_var("PATH", format!("{}:{}", mock_dir.path().display(), original_path));
    f();
    env::set_var("PATH", original_path);
}
```

## Async Tests

Use `#[tokio::test]` for async code:

```rust
#[tokio::test]
async fn test_download() {
    // ...
}
```

## Test Naming

- `test_<function>_<scenario>` for unit tests
- `test_<command>_<scenario>` for integration tests
- Be descriptive: `test_parse_config_missing_toolchain_section`

## Coverage

Aim for:
- 80%+ coverage on `hx-config`, `hx-lock`
- Integration coverage for all CLI commands
- Edge case coverage for error handling

## CI Requirements

All tests must pass on:
- ubuntu-latest (primary)
- macos-latest
- windows-latest (best effort)

## Running Tests

```bash
# All tests
cargo test --workspace

# Specific crate
cargo test -p hx-config

# With output
cargo test -- --nocapture

# Single test
cargo test test_parse_config
```
