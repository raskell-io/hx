# Steel API Reference

The hx plugin system uses [Steel](https://github.com/mattwparas/steel), a Scheme-like language. All hx functions are registered under the `hx/` namespace.

## Project Functions

Access project information and context.

### `(hx/project-name)`

Get the current project name.

```scheme
(define name (hx/project-name))
(hx/info (string-append "Building: " name))
```

**Returns**: String or `#f` if not in a project.

### `(hx/project-root)`

Get the project root directory path.

```scheme
(define root (hx/project-root))
(hx/info (string-append "Root: " root))
```

**Returns**: String (absolute path) or `#f` if not in a project.

### `(hx/ghc-version)`

Get the GHC version being used.

```scheme
(if (hx/ghc-version)
    (hx/info (string-append "Using GHC " (hx/ghc-version)))
    (hx/warn "GHC version unknown"))
```

**Returns**: String (e.g., "9.8.2") or `#f` if unknown.

### `(hx/cabal-file)`

Get the path to the .cabal file.

```scheme
(define cabal (hx/cabal-file))
(if cabal
    (hx/info (string-append "Cabal file: " cabal))
    (hx/warn "No cabal file found"))
```

**Returns**: String (path) or `#f` if not found.

## Shell Functions

Execute external commands.

### `(hx/run cmd args)`

Run a command and get full results.

```scheme
(define result (hx/run "hlint" '("src/")))
;; result is: ((exit-code . 0) (stdout . "...") (stderr . "..."))
```

**Parameters**:
- `cmd` - Command name (String)
- `args` - List of arguments (List of Strings)

**Returns**: Association list with `exit-code`, `stdout`, `stderr`.

**Example - Parse results**:
```scheme
(define (get-exit-code result)
  (cadr (assoc 'exit-code result)))

(define (get-stdout result)
  (cadr (assoc 'stdout result)))

(define result (hx/run "ls" '("-la")))
(if (= 0 (get-exit-code result))
    (hx/info (get-stdout result))
    (hx/error "Command failed"))
```

### `(hx/run-checked cmd args)`

Run a command and return stdout, or raise an error on failure.

```scheme
;; Returns stdout on success, raises error on failure
(define output (hx/run-checked "fourmolu" '("--mode" "check" "src/")))
(hx/info output)
```

**Parameters**:
- `cmd` - Command name (String)
- `args` - List of arguments (List of Strings)

**Returns**: String (stdout) on success.
**Raises**: Error if command fails.

### `(hx/run-silent cmd args)`

Run a command silently and return just the exit code.

```scheme
(define code (hx/run-silent "fourmolu" '("--mode" "check" "src/")))
(if (= 0 code)
    (hx/status "Format" "OK")
    (hx/error "Format check failed"))
```

**Parameters**:
- `cmd` - Command name (String)
- `args` - List of arguments (List of Strings)

**Returns**: Integer (exit code, or -1 on error).

## Filesystem Functions

Read, write, and query files.

### `(hx/read-file path)`

Read a file's contents.

```scheme
(define content (hx/read-file "src/Main.hs"))
(hx/info (string-append "File has "
                        (number->string (string-length content))
                        " characters"))
```

**Parameters**:
- `path` - File path (relative to project root or absolute)

**Returns**: String (file contents).
**Raises**: Error if file cannot be read.

### `(hx/write-file path content)`

Write content to a file. Creates parent directories if needed.

```scheme
(hx/write-file "generated/output.txt" "Hello, World!")
```

**Parameters**:
- `path` - File path (relative to project root or absolute)
- `content` - String to write

**Returns**: void

### `(hx/file-exists? path)`

Check if a file or directory exists.

```scheme
(if (hx/file-exists? "stack.yaml")
    (hx/warn "Found stack.yaml - this project uses Stack")
    (hx/info "No stack.yaml found"))
```

**Parameters**:
- `path` - File path (relative to project root or absolute)

**Returns**: Boolean

### `(hx/glob pattern)`

Find files matching a glob pattern.

```scheme
(define hs-files (hx/glob "src/**/*.hs"))
(hx/info (string-append "Found " (number->string (length hs-files)) " Haskell files"))

(for-each (lambda (f) (hx/debug f)) hs-files)
```

**Parameters**:
- `pattern` - Glob pattern (e.g., `"*.hs"`, `"src/**/*.hs"`)

**Returns**: List of Strings (matching paths).

### `(hx/path-join parts)`

Join path components.

```scheme
(define full-path (hx/path-join (list (hx/project-root) "src" "Main.hs")))
;; e.g., "/home/user/project/src/Main.hs"
```

**Parameters**:
- `parts` - List of path components (List of Strings)

**Returns**: String (joined path).

### `(hx/mkdir path)`

Create a directory (and parents).

```scheme
(hx/mkdir "dist/artifacts")
```

**Parameters**:
- `path` - Directory path (relative to project root or absolute)

**Returns**: void

## Output Functions

Print messages with consistent formatting.

### `(hx/status action message)`

Print a status message with action label.

```scheme
(hx/status "Lint" "Running hlint...")
;; Output: [Lint] Running hlint...
```

**Parameters**:
- `action` - Action name (String, shown in green)
- `message` - Message text (String)

### `(hx/info message)`

Print an informational message.

```scheme
(hx/info "Compiling 15 modules...")
```

### `(hx/warn message)`

Print a warning message (yellow).

```scheme
(hx/warn "Deprecated function used in Main.hs")
;; Output: warning: Deprecated function used in Main.hs
```

### `(hx/error message)`

Print an error message (red).

```scheme
(hx/error "Build failed with 3 errors")
;; Output: error: Build failed with 3 errors
```

### `(hx/debug message)`

Print a debug message (only shown in verbose mode).

```scheme
(hx/debug "Cache hit for aeson-2.2.1.0")
;; Only printed if --verbose flag is set
```

## Complete Example

A full pre-build hook that runs linting and formatting checks:

```scheme
;; plugins/lint.scm

(define (on-pre-build)
  (hx/status "Lint" "Running pre-build checks...")

  ;; Run hlint
  (let ((result (hx/run "hlint" (list "src/"))))
    (let ((exit-code (cadr (assoc 'exit-code result)))
          (stdout (cadr (assoc 'stdout result))))
      (if (= 0 exit-code)
          (hx/info "hlint: OK")
          (begin
            (hx/warn "hlint found issues:")
            (hx/info stdout)))))

  ;; Check formatting
  (let ((format-code (hx/run-silent "fourmolu"
                                    (list "--mode" "check" "src/"))))
    (if (= 0 format-code)
        (hx/info "fourmolu: OK")
        (hx/warn "Code is not formatted - run 'fourmolu -i src/'")))

  ;; Always return success (don't block build)
  #t)
```

## Error Handling

Steel uses `#f` for failure returns and exceptions for errors:

```scheme
;; Check for #f returns
(define ghc (hx/ghc-version))
(if (not ghc)
    (hx/error "GHC version unknown")
    (hx/info ghc))

;; Use try/catch for exceptions
(define (safe-read path)
  (with-exception-handler
    (lambda (e)
      (hx/error (string-append "Failed to read: " path))
      #f)
    (lambda ()
      (hx/read-file path))))
```

## Path Resolution

Relative paths are resolved relative to the project root:

```scheme
;; These are equivalent:
(hx/read-file "src/Main.hs")
(hx/read-file (hx/path-join (list (hx/project-root) "src" "Main.hs")))

;; Absolute paths are used as-is:
(hx/read-file "/etc/hosts")
```
