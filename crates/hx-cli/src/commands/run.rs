//! Run command implementation.

use anyhow::Result;
use hx_cabal::build as cabal_build;
use hx_config::{Project, find_project_root};
use hx_plugins::HookEvent;
use hx_toolchain::{AutoInstallPolicy, Toolchain, ensure_toolchain};
use hx_ui::Output;

use crate::plugins::PluginHooks;

/// Run the project.
pub async fn run(
    args: Vec<String>,
    package: Option<String>,
    policy: AutoInstallPolicy,
    output: &Output,
) -> Result<i32> {
    // Find project root
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    // Check toolchain requirements
    let toolchain = Toolchain::detect().await;
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

    let build_dir = project.cabal_build_dir();

    let exit_code =
        cabal_build::run(&project.root, &build_dir, &args, package.as_deref(), output).await?;

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
    if let Err(e) = check_toolchain(&project, policy).await {
        output.print_error(&e);
        return Ok(4); // Toolchain error exit code
    }

    output.status("Starting", "REPL");

    let build_dir = project.cabal_build_dir();

    let exit_code = cabal_build::repl(&project.root, &build_dir).await?;
    Ok(exit_code)
}

/// Check toolchain requirements and install if needed.
async fn check_toolchain(project: &Project, policy: AutoInstallPolicy) -> hx_core::Result<()> {
    let toolchain = Toolchain::detect().await;

    ensure_toolchain(
        &toolchain,
        project.manifest.toolchain.ghc.as_deref(),
        project.manifest.toolchain.cabal.as_deref(),
        policy,
    )
    .await
}
