# Tracing Spans

`hx-telemetry` uses the `tracing` crate for structured logging and spans.

## What are Spans?

Spans represent a period of time during which an operation is active. They provide structured context that follows through nested operations.

## Creating Spans

### Using the `#[instrument]` Attribute

The easiest way to create spans:

```rust
use tracing::info;

#[tracing::instrument]
pub async fn build(project: &str) -> Result<()> {
    info!("Starting build");
    // Function body becomes a span
    Ok(())
}
```

Output:
```
  INFO build{project="my-project"}: Starting build
```

### Skip Sensitive or Large Arguments

```rust
#[tracing::instrument(skip(config))]
pub async fn build(project: &str, config: &Config) -> Result<()> {
    // config is not captured in the span
}

#[tracing::instrument(skip_all, fields(project = %project_name))]
pub async fn build_all(project_name: &str, packages: &[Package]) -> Result<()> {
    // Skip all args, manually specify fields
}
```

### Manual Span Creation

For more control:

```rust
use tracing::{info, info_span};

fn process_package(name: &str) {
    let span = info_span!("process", package = %name);
    let _guard = span.enter();

    info!("Processing...");
    // Logs show: process{package=foo}: Processing...
}
```

### Async Spans

Use `.instrument()` for async code:

```rust
use tracing::Instrument;

async fn fetch_package(name: &str) -> Result<Package> {
    let span = info_span!("fetch", package = %name);

    async {
        // Async work here
    }
    .instrument(span)
    .await
}
```

## Span Levels

Choose the appropriate span level:

| Level | Usage |
|-------|-------|
| `error_span!` | Failed operations, recovery attempts |
| `warn_span!` | Degraded operations, fallbacks |
| `info_span!` | Normal operation milestones |
| `debug_span!` | Detailed operation tracking |
| `trace_span!` | Very verbose internal operations |

## Span Fields

Add structured data to spans:

```rust
use tracing::info_span;

let span = info_span!(
    "build",
    package = %name,
    version = %version,
    target = "x86_64-linux",
    optimized = true,
);
```

### Field Types

```rust
// Display formatting
span!("op", value = %display_value);

// Debug formatting
span!("op", value = ?debug_value);

// Empty fields (filled later)
let span = info_span!("op", result = tracing::field::Empty);
span.record("result", &"success");
```

## Nested Spans

Spans nest automatically:

```rust
#[tracing::instrument]
async fn build_project() {
    info!("Starting project build");

    for package in packages {
        build_package(package).await;
    }
}

#[tracing::instrument]
async fn build_package(name: &str) {
    info!("Compiling");
}
```

Output:
```
  INFO build_project: Starting project build
  INFO build_project:build_package{name="lib-a"}: Compiling
  INFO build_project:build_package{name="lib-b"}: Compiling
```

## Span Events

Log events within spans:

```rust
#[tracing::instrument]
async fn resolve_deps() {
    info!("Starting resolution");

    // ... work ...

    if conflict {
        warn!("Version conflict detected");
    }

    info!("Resolution complete");
}
```

## Timing with TimingGuard

Automatically log duration:

```rust
use hx_telemetry::TimingGuard;

fn expensive_operation() {
    let _timer = TimingGuard::new("expensive_operation");
    // ... work ...
}
// Logs: expensive_operation completed in 1234ms
```

## Timing with `time!` Macro

```rust
use hx_telemetry::time;

let result = time!("resolve_deps", {
    resolver.resolve(&constraints)?
});
// Logs: resolve_deps completed in 500ms
```

## Best Practices

### 1. Name Spans Descriptively

```rust
// Good
info_span!("build_package", name = %pkg.name)
info_span!("download_tarball", url = %url)

// Bad
info_span!("op1")
info_span!("process")
```

### 2. Include Relevant Context

```rust
// Good - includes useful context
info_span!("resolve",
    package = %name,
    constraint = %constraint,
    num_candidates = candidates.len()
)

// Bad - missing useful context
info_span!("resolve")
```

### 3. Use Appropriate Levels

```rust
// High-level operations: info
#[tracing::instrument(level = "info")]
async fn build() { }

// Implementation details: debug
#[tracing::instrument(level = "debug")]
fn parse_cabal_file() { }

// Internal mechanics: trace
#[tracing::instrument(level = "trace")]
fn compute_fingerprint() { }
```

### 4. Skip Large Data

```rust
// Don't capture large or sensitive data
#[tracing::instrument(skip(file_contents, api_key))]
fn process(file_contents: &str, api_key: &str) { }
```

### 5. Handle Errors in Spans

```rust
#[tracing::instrument(err)]
async fn fetch() -> Result<Data> {
    // Automatically logs errors as events
}

// Or manually:
#[tracing::instrument]
async fn fetch() -> Result<Data> {
    match do_fetch().await {
        Ok(data) => Ok(data),
        Err(e) => {
            tracing::error!(error = %e, "Fetch failed");
            Err(e)
        }
    }
}
```

## Viewing Spans

### Text Output

Default format shows span context:

```
  INFO build{project="foo"}: Starting
  DEBUG build{project="foo"}:resolve: Finding candidates
  INFO build{project="foo"}: Complete in 2.3s
```

### JSON Output

With `HX_LOG_JSON=1`:

```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "level": "INFO",
  "target": "hx_cabal::build",
  "span": {
    "name": "build",
    "project": "foo"
  },
  "message": "Starting"
}
```

### Span Timing

Enable span timing with `FmtSpan::CLOSE`:

```rust
fmt::layer()
    .with_span_events(FmtSpan::CLOSE)
```

Output includes duration when span closes:
```
  INFO build{project="foo"}: close time.busy=2.3s time.idle=100ms
```

## Example: Full Instrumentation

```rust
use tracing::{info, debug, warn, instrument};
use hx_telemetry::TimingGuard;

#[instrument(skip(config))]
pub async fn build_project(
    project_name: &str,
    config: &Config,
) -> Result<BuildResult> {
    let _timer = TimingGuard::new("build_project");

    info!("Starting build");

    // Resolve dependencies
    let deps = resolve_dependencies(project_name).await?;
    debug!(num_deps = deps.len(), "Dependencies resolved");

    // Build each package
    for pkg in &deps {
        if let Err(e) = build_package(pkg).await {
            warn!(package = %pkg.name, error = %e, "Build failed, continuing...");
        }
    }

    info!("Build complete");
    Ok(BuildResult::success())
}

#[instrument(level = "debug")]
async fn resolve_dependencies(project: &str) -> Result<Vec<Package>> {
    // ...
}

#[instrument(level = "debug", fields(package = %pkg.name))]
async fn build_package(pkg: &Package) -> Result<()> {
    // ...
}
```
