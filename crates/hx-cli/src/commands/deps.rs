//! Dependency management commands (add, rm, graph).

use crate::cli::GraphFormat;
use anyhow::Result;
use hx_cabal::{CabalEditError, add_dependency, remove_dependency};
use hx_config::{Project, find_project_root};
use hx_lock::Lockfile;
use hx_ui::Output;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::Write;

/// Add a dependency to the project.
pub async fn add(package: &str, _dev: bool, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            output.info("Run `hx init` to create a new project first.");
            return Ok(3);
        }
    };

    // Find the .cabal file
    let cabal_file = find_cabal_file(&project_root);
    let cabal_file = match cabal_file {
        Some(path) => path,
        None => {
            output.error("No .cabal file found in project");
            output.info("Make sure your project has a .cabal file.");
            return Ok(3);
        }
    };

    output.status(
        "Adding",
        &format!(
            "{} to {}",
            package,
            cabal_file.file_name().unwrap().to_string_lossy()
        ),
    );

    match add_dependency(&cabal_file, package, None) {
        Ok(()) => {
            output.status("Added", package);
            Ok(0)
        }
        Err(CabalEditError::DependencyExists(_)) => {
            output.warn(&format!("Dependency '{}' already exists", package));
            Ok(0)
        }
        Err(CabalEditError::NoBuildDepends) => {
            output.error("No build-depends section found in .cabal file");
            output.info("Make sure your .cabal file has a build-depends section.");
            Ok(3)
        }
        Err(e) => {
            output.error(&format!("Failed to add dependency: {}", e));
            Ok(1)
        }
    }
}

/// Remove a dependency from the project.
pub async fn remove(package: &str, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            output.info("Run `hx init` to create a new project first.");
            return Ok(3);
        }
    };

    // Find the .cabal file
    let cabal_file = find_cabal_file(&project_root);
    let cabal_file = match cabal_file {
        Some(path) => path,
        None => {
            output.error("No .cabal file found in project");
            output.info("Make sure your project has a .cabal file.");
            return Ok(3);
        }
    };

    output.status(
        "Removing",
        &format!(
            "{} from {}",
            package,
            cabal_file.file_name().unwrap().to_string_lossy()
        ),
    );

    match remove_dependency(&cabal_file, package) {
        Ok(()) => {
            output.status("Removed", package);
            Ok(0)
        }
        Err(CabalEditError::DependencyNotFound(_)) => {
            output.warn(&format!("Dependency '{}' not found", package));
            Ok(0)
        }
        Err(CabalEditError::NoBuildDepends) => {
            output.error("No build-depends section found in .cabal file");
            Ok(3)
        }
        Err(e) => {
            output.error(&format!("Failed to remove dependency: {}", e));
            Ok(1)
        }
    }
}

/// Show dependency graph.
pub async fn graph(
    format: GraphFormat,
    depth: usize,
    highlight: Option<String>,
    output_file: Option<String>,
    _dev: bool,
    direct_only: bool,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = match find_project_root(".") {
        Ok(root) => root,
        Err(_) => {
            output.error("Not in an hx project (no hx.toml found)");
            return Ok(3);
        }
    };

    let project = Project::load(&project_root)?;

    // Load lockfile to get dependencies
    let lockfile_path = project.lockfile_path();
    let lockfile = match Lockfile::from_file(&lockfile_path) {
        Ok(lf) => lf,
        Err(_) => {
            output.error("No lockfile found");
            output.info("Run `hx lock` to generate a lockfile first.");
            return Ok(3);
        }
    };

    // Build dependency graph from lockfile
    let graph = build_dep_graph(&lockfile, direct_only);

    // Parse highlight packages
    let highlights: HashSet<String> = highlight
        .map(|h| h.split(',').map(|s| s.trim().to_string()).collect())
        .unwrap_or_default();

    // Generate output
    let result = match format {
        GraphFormat::Dot => generate_dot(&graph, &project, &highlights),
        GraphFormat::Tree => generate_tree(&graph, &project, depth, &highlights),
        GraphFormat::List => generate_list(&graph, &highlights),
        GraphFormat::Json => generate_json(&graph),
    };

    // Write output
    if let Some(file_path) = output_file {
        let mut file = fs::File::create(&file_path)?;
        file.write_all(result.as_bytes())?;
        output.status("Written", &file_path);
    } else {
        println!("{}", result);
    }

    Ok(0)
}

/// Dependency graph representation.
#[derive(Debug)]
struct DepGraph {
    /// Package name -> version
    packages: HashMap<String, String>,
    /// Package name -> dependencies
    edges: HashMap<String, Vec<String>>,
    /// Direct dependencies (from .cabal)
    direct: HashSet<String>,
}

/// Build dependency graph from lockfile.
fn build_dep_graph(lockfile: &Lockfile, direct_only: bool) -> DepGraph {
    let mut packages = HashMap::new();
    let mut edges: HashMap<String, Vec<String>> = HashMap::new();
    let mut direct = HashSet::new();

    // Add all packages from lockfile
    for pkg in &lockfile.packages {
        packages.insert(pkg.name.clone(), pkg.version.clone());
        edges.entry(pkg.name.clone()).or_default();
    }

    // For now, we don't have full dependency edges in the lockfile,
    // so we'll mark all as direct dependencies
    // In a full implementation, we'd parse the .cabal file or use resolver data
    for pkg in &lockfile.packages {
        direct.insert(pkg.name.clone());
    }

    if direct_only {
        // Filter to only direct dependencies
        let direct_pkgs: HashSet<_> = direct.iter().cloned().collect();
        packages.retain(|k, _| direct_pkgs.contains(k));
        edges.retain(|k, _| direct_pkgs.contains(k));
    }

    DepGraph {
        packages,
        edges,
        direct,
    }
}

/// Generate Graphviz DOT format.
fn generate_dot(graph: &DepGraph, project: &Project, highlights: &HashSet<String>) -> String {
    let mut dot = String::new();

    dot.push_str("digraph dependencies {\n");
    dot.push_str("    rankdir=LR;\n");
    dot.push_str("    node [shape=box, fontname=\"Helvetica\"];\n");
    dot.push_str("    edge [color=\"#666666\"];\n\n");

    // Add project node
    dot.push_str(&format!(
        "    \"{}\" [label=\"{}\\n(project)\", style=filled, fillcolor=\"#4a90d9\", fontcolor=white];\n",
        project.name(),
        project.name()
    ));

    // Add package nodes
    for (name, version) in &graph.packages {
        let is_highlighted = highlights.contains(name);
        let is_direct = graph.direct.contains(name);

        let (fill_color, font_color) = if is_highlighted {
            ("#ffd700", "black") // Gold for highlighted
        } else if is_direct {
            ("#90EE90", "black") // Light green for direct
        } else {
            ("#f0f0f0", "black") // Light gray for transitive
        };

        dot.push_str(&format!(
            "    \"{}\" [label=\"{}\\n{}\", style=filled, fillcolor=\"{}\", fontcolor={}];\n",
            name, name, version, fill_color, font_color
        ));
    }

    dot.push('\n');

    // Add edges from project to direct dependencies
    for dep in &graph.direct {
        dot.push_str(&format!("    \"{}\" -> \"{}\";\n", project.name(), dep));
    }

    // Add edges between packages
    for (pkg, deps) in &graph.edges {
        for dep in deps {
            if graph.packages.contains_key(dep) {
                dot.push_str(&format!("    \"{}\" -> \"{}\";\n", pkg, dep));
            }
        }
    }

    dot.push_str("}\n");
    dot
}

/// Generate ASCII tree format.
fn generate_tree(
    graph: &DepGraph,
    project: &Project,
    max_depth: usize,
    highlights: &HashSet<String>,
) -> String {
    let mut result = String::new();

    // Project header
    result.push_str(&format!("{}\n", project.name()));

    // Sort dependencies for consistent output
    let mut deps: Vec<_> = graph.direct.iter().collect();
    deps.sort();

    let dep_count = deps.len();
    for (i, dep) in deps.iter().enumerate() {
        let is_last = i == dep_count - 1;
        let prefix = if is_last { "└── " } else { "├── " };
        let cont_prefix = if is_last { "    " } else { "│   " };

        let version = graph.packages.get(*dep).map(|v| v.as_str()).unwrap_or("?");
        let highlight_marker = if highlights.contains(*dep) { " *" } else { "" };

        result.push_str(&format!("{}{} {}{}\n", prefix, dep, version, highlight_marker));

        // Print transitive dependencies if depth allows
        if (max_depth == 0 || max_depth > 1)
            && let Some(trans_deps) = graph.edges.get(*dep)
        {
            print_tree_deps(
                &mut result,
                trans_deps,
                graph,
                highlights,
                cont_prefix,
                if max_depth == 0 { 0 } else { max_depth - 1 },
                1,
            );
        }
    }

    result
}

/// Recursively print tree dependencies.
fn print_tree_deps(
    result: &mut String,
    deps: &[String],
    graph: &DepGraph,
    highlights: &HashSet<String>,
    prefix: &str,
    max_depth: usize,
    current_depth: usize,
) {
    if max_depth > 0 && current_depth >= max_depth {
        return;
    }

    let mut sorted_deps: Vec<_> = deps.iter().filter(|d| graph.packages.contains_key(*d)).collect();
    sorted_deps.sort();

    let dep_count = sorted_deps.len();
    for (i, dep) in sorted_deps.iter().enumerate() {
        let is_last = i == dep_count - 1;
        let branch = if is_last { "└── " } else { "├── " };
        let cont = if is_last { "    " } else { "│   " };

        let version = graph.packages.get(*dep).map(|v| v.as_str()).unwrap_or("?");
        let highlight_marker = if highlights.contains(*dep) { " *" } else { "" };

        result.push_str(&format!(
            "{}{}{} {}{}\n",
            prefix, branch, dep, version, highlight_marker
        ));

        // Recurse for transitive deps
        if let Some(trans_deps) = graph.edges.get(*dep) {
            let new_prefix = format!("{}{}", prefix, cont);
            print_tree_deps(
                result,
                trans_deps,
                graph,
                highlights,
                &new_prefix,
                max_depth,
                current_depth + 1,
            );
        }
    }
}

/// Generate simple list format.
fn generate_list(graph: &DepGraph, highlights: &HashSet<String>) -> String {
    let mut result = String::new();

    // Sort packages alphabetically
    let mut packages: Vec<_> = graph.packages.iter().collect();
    packages.sort_by(|a, b| a.0.cmp(b.0));

    for (name, version) in packages {
        let marker = if highlights.contains(name) { " *" } else { "" };
        let direct_marker = if graph.direct.contains(name) {
            " (direct)"
        } else {
            ""
        };
        result.push_str(&format!("{} {}{}{}\n", name, version, direct_marker, marker));
    }

    result
}

/// Generate JSON format.
fn generate_json(graph: &DepGraph) -> String {
    let mut json = String::new();
    json.push_str("{\n");
    json.push_str("  \"packages\": [\n");

    let mut packages: Vec<_> = graph.packages.iter().collect();
    packages.sort_by(|a, b| a.0.cmp(b.0));

    let pkg_count = packages.len();
    for (i, (name, version)) in packages.iter().enumerate() {
        let deps = graph.edges.get(*name).cloned().unwrap_or_default();
        let deps_json: Vec<String> = deps.iter().map(|d| format!("\"{}\"", d)).collect();
        let is_direct = graph.direct.contains(*name);

        json.push_str("    {\n");
        json.push_str(&format!("      \"name\": \"{}\",\n", name));
        json.push_str(&format!("      \"version\": \"{}\",\n", version));
        json.push_str(&format!("      \"direct\": {},\n", is_direct));
        json.push_str(&format!("      \"dependencies\": [{}]\n", deps_json.join(", ")));
        json.push_str(&format!(
            "    }}{}",
            if i < pkg_count - 1 { ",\n" } else { "\n" }
        ));
    }

    json.push_str("  ]\n");
    json.push_str("}\n");
    json
}

/// Find a .cabal file in the given directory.
fn find_cabal_file(dir: &std::path::Path) -> Option<std::path::PathBuf> {
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "cabal") {
            return Some(path);
        }
    }
    None
}
