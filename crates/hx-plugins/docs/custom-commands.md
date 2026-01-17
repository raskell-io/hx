# Custom Commands

Plugins can register custom commands that appear as `hx <command>`.

## Defining Commands

### In Rust

Use the `CustomCommand` struct:

```rust
use hx_plugins::{CustomCommand, CommandBuilder};

// Simple construction
let cmd = CustomCommand::new("deploy")
    .with_description("Deploy the application")
    .with_usage("hx deploy [--env <environment>]")
    .with_source("deploy-plugin");

// Using the builder
let cmd = CommandBuilder::new("deploy")
    .description("Deploy the application")
    .usage("hx deploy [--env <environment>]")
    .source("deploy-plugin")
    .build();
```

### In Steel

Commands are registered using the prelude API:

```scheme
;; plugins/deploy.scm

(hx/register-command
  "deploy"
  "Deploy the application to production"
  (lambda (args)
    (hx/status "Deploy" "Starting deployment...")

    ;; Parse arguments
    (define env (if (member "--prod" args) "production" "staging"))

    ;; Run deployment
    (let ((result (hx/run "deploy.sh" (list env))))
      (if (= 0 (cadr (assoc 'exit-code result)))
          (begin
            (hx/info "Deployment successful")
            0)
          (begin
            (hx/error "Deployment failed")
            1)))))
```

## CustomCommand Structure

```rust
pub struct CustomCommand {
    /// Command name (what users type after `hx`)
    pub name: String,

    /// Description shown in help
    pub description: String,

    /// Usage string for help
    pub usage: Option<String>,

    /// Source plugin that registered this command
    pub source_plugin: Option<String>,
}
```

## Running Custom Commands

### Via Plugin Manager

```rust
use hx_plugins::{PluginManager, PluginConfig};

let mut manager = PluginManager::new(config)?;
manager.initialize()?;
manager.load_all(&project_root)?;

// Check if command exists
if manager.has_command("deploy") {
    // Run the command
    let exit_code = manager.run_command("deploy", &["--prod".to_string()])?;
    std::process::exit(exit_code);
}
```

### Via CLI

```bash
# Run custom command
hx deploy --prod

# Get help
hx deploy --help
```

## Command Discovery

List all available custom commands:

```rust
let commands = manager.commands();
for (name, cmd) in commands {
    println!("  {} - {}", name, cmd.description);
}
```

## Help Text Generation

Commands automatically generate help text:

```rust
let help = command.help_text();
// Output:
// Deploy the application
//
// Usage: hx deploy [--env <environment>]
//
// (Defined by: deploy-plugin)
```

## Best Practices

### 1. Clear Naming

Choose command names that are:
- Short but descriptive
- Don't conflict with built-in commands
- Use lowercase with hyphens

```scheme
;; Good
"run-tests"
"deploy"
"check-deps"

;; Bad
"rt"           ; Too cryptic
"Build"        ; Conflicts with built-in
"runAllTests"  ; Use hyphens
```

### 2. Argument Handling

Parse arguments consistently:

```scheme
(define (parse-args args)
  (define env "staging")
  (define verbose #f)

  (for-each
    (lambda (arg)
      (cond
        ((string=? arg "--prod") (set! env "production"))
        ((string=? arg "--staging") (set! env "staging"))
        ((string=? arg "-v") (set! verbose #t))))
    args)

  (list (cons 'env env) (cons 'verbose verbose)))
```

### 3. Exit Codes

Return meaningful exit codes:

```scheme
(define (my-command args)
  (cond
    ((check-success?) 0)    ; Success
    ((check-warning?) 0)    ; Warning but OK
    ((check-error?) 1)      ; General error
    (else 1)))              ; Unknown error
```

### 4. User Feedback

Provide clear status updates:

```scheme
(define (deploy args)
  (hx/status "Deploy" "Validating configuration...")

  (if (not (valid-config?))
      (begin
        (hx/error "Invalid configuration")
        (hx/info "Run 'hx deploy --check' to validate")
        1)
      (begin
        (hx/status "Deploy" "Building artifacts...")
        ;; ... continue
        )))
```

### 5. Documentation

Include usage information:

```rust
CustomCommand::new("deploy")
    .with_description("Deploy the application to a target environment")
    .with_usage("hx deploy [OPTIONS]\n\n\
                 Options:\n\
                   --prod       Deploy to production\n\
                   --staging    Deploy to staging (default)\n\
                   --dry-run    Show what would be deployed\n\
                   -v           Verbose output")
```

## Example: Full Custom Command

```scheme
;; plugins/release.scm
;; Custom command for creating releases

(define (on-init)
  (hx/register-command
    "release"
    "Create a new release"
    release-command))

(define (release-command args)
  (define version (if (null? args) #f (car args)))

  (if (not version)
      (begin
        (hx/error "Usage: hx release <version>")
        (hx/info "Example: hx release 1.2.0")
        2)  ; Usage error
      (create-release version)))

(define (create-release version)
  (hx/status "Release" (string-append "Creating release v" version))

  ;; 1. Run tests
  (hx/info "Running tests...")
  (let ((test-result (hx/run-silent "hx" '("test"))))
    (if (not (= 0 test-result))
        (begin
          (hx/error "Tests failed - aborting release")
          1)
        (continue-release version))))

(define (continue-release version)
  ;; 2. Update version in .cabal
  (hx/status "Release" "Updating version...")
  (let ((cabal (hx/cabal-file)))
    (if cabal
        (update-cabal-version cabal version)
        (hx/warn "No .cabal file found")))

  ;; 3. Create git tag
  (hx/status "Release" "Creating git tag...")
  (let ((tag-result (hx/run "git" (list "tag" "-a"
                                         (string-append "v" version)
                                         "-m" (string-append "Release " version)))))
    (if (= 0 (cadr (assoc 'exit-code tag-result)))
        (begin
          (hx/info (string-append "Created tag v" version))
          (hx/info "Run 'git push --tags' to publish")
          0)
        (begin
          (hx/error "Failed to create tag")
          1))))

(define (update-cabal-version cabal-file version)
  (let ((content (hx/read-file cabal-file)))
    ;; Simple version replacement
    ;; In real code, use proper parsing
    (hx/debug (string-append "Would update " cabal-file " to " version))))
```

## Integration with CLI

Custom commands integrate with the hx CLI:

```bash
# List commands (including custom)
hx --help

# Run custom command
hx release 1.0.0

# Custom command with flags
hx deploy --prod --verbose
```

The CLI automatically routes unknown commands to the plugin system.
