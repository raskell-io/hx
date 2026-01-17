//! Run command implementation.

use anyhow::Result;
use hx_cabal::build as cabal_build;
use hx_config::{CompilerBackend, Project, find_project_root};
use hx_plugins::HookEvent;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::Output;

use crate::plugins::PluginHooks;

/// Run the project.
pub async fn run(
    args: Vec<String>,
    package: Option<String>,
    target: Option<String>,
    backend_override: Option<CompilerBackend>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Determine compiler backend (CLI override > config > default)
    let backend = backend_override.unwrap_or(project.manifest.compiler.backend);

    // BHC run support would go here
    if backend == CompilerBackend::Bhc {
        output.warn("BHC run support is not yet fully implemented, falling back to GHC");
    }

    // Check toolchain requirements
    let mut toolchain = Toolchain::detect().await;
    if let Err(e) = ensure_toolchain(
        &toolchain,
        project.manifest.toolchain.ghc.as_deref(),
        project.manifest.toolchain.cabal.as_deref(),
        policy,
    )
    .await
    {
        output.print_error(&e);
        return Ok(4); // Toolchain error exit code
    }

    // Re-detect toolchain after potential installation to get updated paths
    if !toolchain.ghc.status.is_found() || !toolchain.cabal.status.is_found() {
        toolchain = Toolchain::detect().await;
    }

    // Validate package selection for workspaces
    if let Some(ref pkg_name) = package {
        if project.is_workspace() {
            if project.get_package(pkg_name).is_none() {
                output.error(&format!("Package '{}' not found in workspace", pkg_name));
                output.info(&format!(
                    "Available packages: {}",
                    project.package_names().join(", ")
                ));
                return Ok(1);
            }
        } else {
            output.warn("--package flag ignored (not a workspace project)");
        }
    }

    let run_target = match &package {
        Some(pkg) if project.is_workspace() => pkg.as_str(),
        _ => project.name(),
    };

    // Get GHC version for plugin context
    let ghc_version = toolchain.ghc.status.version().map(|v| v.to_string());

    // Initialize plugin hooks
    let mut hooks = PluginHooks::from_project(&project, ghc_version);
    if let Some(ref mut h) = hooks {
        if let Err(e) = h.initialize() {
            output.verbose(&format!("Plugin initialization warning: {}", e));
        }
    }

    // Run pre-run hooks
    if let Some(ref mut h) = hooks {
        if !h.run_pre_hook(HookEvent::PreRun, output) {
            output.error("Pre-run hook failed");
            return Ok(6); // Hook failure exit code
        }
    }

    output.status("Running", run_target);

    // Collect toolchain bin directories for PATH
    let mut toolchain_bin_dirs = Vec::new();
    if let Some(ghc_path) = toolchain.ghc.status.path() {
        if let Some(parent) = ghc_path.parent() {
            toolchain_bin_dirs.push(parent.to_path_buf());
        }
    }
    if let Some(cabal_path) = toolchain.cabal.status.path() {
        if let Some(parent) = cabal_path.parent() {
            toolchain_bin_dirs.push(parent.to_path_buf());
        }
    }

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::run(
        &project.root,
        &build_dir,
        &args,
        package.as_deref(),
        target.as_deref(),
        &toolchain_bin_dirs,
        output,
    )
    .await?;

    // Run post-run hooks
    if let Some(ref mut h) = hooks {
        h.run_post_hook(HookEvent::PostRun, output);
    }

    Ok(exit_code)
}

/// Start a REPL.
pub async fn repl(policy: AutoInstallPolicy, output: &Output) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Check toolchain requirements
    let mut toolchain = Toolchain::detect().await;
    if let Err(e) = ensure_toolchain(
        &toolchain,
        project.manifest.toolchain.ghc.as_deref(),
        project.manifest.toolchain.cabal.as_deref(),
        policy,
    )
    .await
    {
        output.print_error(&e);
        return Ok(4); // Toolchain error exit code
    }

    // Re-detect toolchain after potential installation to get updated paths
    if !toolchain.ghc.status.is_found() || !toolchain.cabal.status.is_found() {
        toolchain = Toolchain::detect().await;
    }

    output.status("Starting", "REPL");

    // Collect toolchain bin directories for PATH
    let mut toolchain_bin_dirs = Vec::new();
    if let Some(ghc_path) = toolchain.ghc.status.path() {
        if let Some(parent) = ghc_path.parent() {
            toolchain_bin_dirs.push(parent.to_path_buf());
        }
    }
    if let Some(cabal_path) = toolchain.cabal.status.path() {
        if let Some(parent) = cabal_path.parent() {
            toolchain_bin_dirs.push(parent.to_path_buf());
        }
    }

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::repl(&project.root, &build_dir, &toolchain_bin_dirs).await?;
    Ok(exit_code)
}
