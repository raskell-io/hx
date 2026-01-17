# Version Constraints

`hx-solver` implements the Haskell Package Versioning Policy (PVP) for version handling.

## PVP Version Format

PVP versions have the format `A.B.C.D`:

| Component | Meaning | Bump when |
|-----------|---------|-----------|
| A.B | Major version | Breaking API changes |
| C | Minor version | Non-breaking additions |
| D | Patch version | Bug fixes only |

```rust
use hx_solver::Version;

let v: Version = "1.2.3.4".parse().unwrap();
assert_eq!(v.major(), (1, 2));  // A.B
assert_eq!(v.minor(), 3);        // C
assert_eq!(v.patch(), 4);        // D
```

## Version Comparison

Versions compare component by component:

```rust
let v1: Version = "1.2.3".parse().unwrap();
let v2: Version = "1.2.4".parse().unwrap();
let v3: Version = "1.3.0".parse().unwrap();

assert!(v1 < v2);   // 1.2.3 < 1.2.4
assert!(v2 < v3);   // 1.2.4 < 1.3.0

// Trailing zeros don't matter
let v4: Version = "1.2.3.0".parse().unwrap();
assert!(v1 == v4);  // 1.2.3 == 1.2.3.0
```

## PVP Compatibility

Two versions are PVP-compatible if they share the same major version (A.B):

```rust
let v1: Version = "1.2.3".parse().unwrap();
let v2: Version = "1.2.8".parse().unwrap();
let v3: Version = "1.3.0".parse().unwrap();

assert!(v1.is_pvp_compatible(&v2));   // Same major (1.2)
assert!(!v1.is_pvp_compatible(&v3));  // Different major (1.2 vs 1.3)
```

## Constraint Types

### Any

Matches any version:

```rust
use hx_solver::VersionConstraint;

let c = VersionConstraint::Any;
assert!(c.matches(&"1.0".parse().unwrap()));
assert!(c.matches(&"999.0".parse().unwrap()));
```

**Syntax**: `-any` or empty

### Exact

Matches exactly one version:

```rust
let c = parse_constraint("== 1.2.3").unwrap();

assert!(c.matches(&"1.2.3".parse().unwrap()));
assert!(!c.matches(&"1.2.4".parse().unwrap()));
```

**Syntax**: `== 1.2.3`

### Comparison

```rust
// Greater than
let c = parse_constraint("> 1.0").unwrap();
assert!(c.matches(&"1.1".parse().unwrap()));
assert!(!c.matches(&"1.0".parse().unwrap()));

// Greater than or equal
let c = parse_constraint(">= 1.0").unwrap();
assert!(c.matches(&"1.0".parse().unwrap()));
assert!(c.matches(&"1.1".parse().unwrap()));

// Less than
let c = parse_constraint("< 2.0").unwrap();
assert!(c.matches(&"1.9".parse().unwrap()));
assert!(!c.matches(&"2.0".parse().unwrap()));

// Less than or equal
let c = parse_constraint("<= 2.0").unwrap();
assert!(c.matches(&"2.0".parse().unwrap()));
assert!(!c.matches(&"2.1".parse().unwrap()));
```

### Caret (PVP Compatible)

The `^>=` operator means "this version or any PVP-compatible newer version":

```rust
let c = parse_constraint("^>= 1.2.3").unwrap();

// Must be >= 1.2.3
assert!(!c.matches(&"1.2.2".parse().unwrap()));
assert!(c.matches(&"1.2.3".parse().unwrap()));
assert!(c.matches(&"1.2.9".parse().unwrap()));

// Must have same major version (1.2)
assert!(!c.matches(&"1.3.0".parse().unwrap()));
assert!(!c.matches(&"2.0.0".parse().unwrap()));
```

**Equivalent to**: `>= 1.2.3 && < 1.3`

### Conjunction (AND)

Both constraints must match:

```rust
let c = parse_constraint(">= 1.0 && < 2.0").unwrap();

assert!(!c.matches(&"0.9".parse().unwrap()));
assert!(c.matches(&"1.0".parse().unwrap()));
assert!(c.matches(&"1.5".parse().unwrap()));
assert!(!c.matches(&"2.0".parse().unwrap()));
```

**Syntax**: `constraint && constraint`

### Disjunction (OR)

Either constraint can match:

```rust
let c = parse_constraint(">= 2.0 || < 1.0").unwrap();

assert!(c.matches(&"0.5".parse().unwrap()));
assert!(!c.matches(&"1.5".parse().unwrap()));
assert!(c.matches(&"2.0".parse().unwrap()));
```

**Syntax**: `constraint || constraint`

## Constraint Syntax Summary

| Syntax | Meaning | Example |
|--------|---------|---------|
| `-any` | Any version | Matches all |
| `== 1.2.3` | Exactly this | Only 1.2.3 |
| `> 1.2` | Greater than | 1.3, 2.0, ... |
| `>= 1.2` | At least | 1.2, 1.3, 2.0, ... |
| `< 2.0` | Less than | 0.1, 1.0, 1.9.9 |
| `<= 2.0` | At most | 0.1, 1.0, 2.0 |
| `^>= 1.2` | PVP compatible | 1.2, 1.2.5, not 1.3 |
| `a && b` | Both must match | Intersection |
| `a \|\| b` | Either matches | Union |

## Parsing Constraints

```rust
use hx_solver::parse_constraint;

// Single constraint
let c = parse_constraint(">= 1.0").unwrap();

// Combined constraints
let c = parse_constraint(">= 1.0 && < 2.0").unwrap();

// Complex constraint
let c = parse_constraint("(>= 1.0 && < 1.5) || >= 2.0").unwrap();

// Error handling
match parse_constraint("invalid") {
    Ok(c) => println!("Parsed: {}", c),
    Err(e) => println!("Error: {}", e),
}
```

## Creating Constraints Programmatically

```rust
use hx_solver::{Version, VersionConstraint};

// Direct construction
let v: Version = "1.2.3".parse().unwrap();
let c = VersionConstraint::GreaterThanOrEqual(v);

// Builder pattern
let c = VersionConstraint::GreaterThanOrEqual("1.0".parse().unwrap())
    .and(VersionConstraint::LessThan("2.0".parse().unwrap()));

// Display for debugging
println!("{}", c);  // >=1.0 && <2.0
```

## Common Patterns

### Version Range

```rust
// Common in .cabal files: >= 1.0 && < 2
let c = parse_constraint(">= 1.0 && < 2.0").unwrap();
```

### Multiple Major Versions

```rust
// Support multiple major versions
let c = parse_constraint("^>= 1.0 || ^>= 2.0").unwrap();
```

### Excluding Specific Versions

```rust
// Exclude a known broken version
let c = parse_constraint(">= 1.0 && < 1.5.2 || > 1.5.2").unwrap();
```

## Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum VersionParseError {
    #[error("empty version string")]
    Empty,
    #[error("invalid version component: {0}")]
    InvalidComponent(String),
}

#[derive(Debug, thiserror::Error)]
pub enum ConstraintParseError {
    #[error("invalid constraint format: {0}")]
    InvalidFormat(String),
    #[error("invalid version: {0}")]
    InvalidVersion(#[from] VersionParseError),
}
```
