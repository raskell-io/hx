# Exit Codes

hx follows standard CLI conventions for exit codes, enabling integration with scripts and CI systems.

## Exit Code Table

| Code | Constant | Meaning |
|------|----------|---------|
| 0 | `ExitCode::Success` | Command completed successfully |
| 1 | `ExitCode::GeneralError` | General/unspecified error |
| 2 | `ExitCode::UsageError` | Invalid command-line arguments |
| 3 | `ExitCode::ConfigError` | Configuration file error |
| 4 | `ExitCode::ToolchainError` | Toolchain not found or wrong version |
| 5 | `ExitCode::BuildError` | Build or test failure |
| 6 | `ExitCode::HookError` | Plugin hook failed |

## ExitCode Enum

```rust
#[repr(i32)]
pub enum ExitCode {
    Success = 0,
    GeneralError = 1,
    UsageError = 2,
    ConfigError = 3,
    ToolchainError = 4,
    BuildError = 5,
    HookError = 6,
}

impl ExitCode {
    pub fn code(&self) -> i32;
}

impl From<ExitCode> for i32 {
    fn from(code: ExitCode) -> i32;
}
```

## Usage in Commands

```rust
use hx_core::ExitCode;

pub async fn build(args: BuildArgs) -> i32 {
    // Check toolchain
    if !toolchain_found() {
        return ExitCode::ToolchainError.into();
    }

    // Load config
    let config = match Config::load() {
        Ok(c) => c,
        Err(_) => return ExitCode::ConfigError.into(),
    };

    // Run build
    match run_build(&config).await {
        Ok(_) => ExitCode::Success.into(),
        Err(_) => ExitCode::BuildError.into(),
    }
}
```

## Error to Exit Code Mapping

```rust
impl From<&Error> for ExitCode {
    fn from(error: &Error) -> Self {
        match error {
            Error::ToolchainMissing { .. } => ExitCode::ToolchainError,
            Error::ToolchainMismatch { .. } => ExitCode::ToolchainError,
            Error::Config { .. } => ExitCode::ConfigError,
            Error::BuildFailed { .. } => ExitCode::BuildError,
            Error::ProjectNotFound { .. } => ExitCode::ConfigError,
            _ => ExitCode::GeneralError,
        }
    }
}
```

## CI Integration

Exit codes enable CI pipeline integration:

```yaml
# GitHub Actions
- name: Build
  run: hx build
  # Exit code 0 = success, non-zero = failure

# Shell scripts
hx build || exit 1
hx test || echo "Tests failed with code $?"
```

## Scripting Examples

### Bash

```bash
#!/bin/bash
set -e

hx build
if [ $? -eq 0 ]; then
    echo "Build succeeded"
else
    echo "Build failed"
    exit 1
fi
```

### Make

```makefile
build:
    hx build

test: build
    hx test

.PHONY: build test
```

## Best Practices

1. **Return specific codes** - Use the most specific exit code for the failure
2. **Document exit codes** - Users should know what each code means
3. **Be consistent** - Same error type = same exit code
4. **Enable scripting** - 0 = success, non-zero = failure
5. **Avoid high codes** - Stay below 128 (shell signal codes)
