//! Lint command implementation.

use anyhow::Result;
use hx_config::find_project_root;
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};

/// Run the lint command.
pub async fn run(fix: bool, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;

    output.status("Linting", &project_root.display().to_string());

    // Check for hlint
    if which::which("hlint").is_err() {
        output.error("hlint not found");
        output.info("Install hlint: cabal install hlint");
        return Ok(4);
    }

    let spinner = Spinner::new("Running hlint...");

    let runner = CommandRunner::new().with_working_dir(&project_root);

    let mut args = vec!["src"];
    if fix {
        args.push("--refactor");
        args.push("--refactor-options=-i");
    }

    let cmd_output = runner.run("hlint", args.iter().copied()).await?;

    if cmd_output.success() {
        spinner.finish_success("No issues found");
        Ok(0)
    } else {
        spinner.finish_warning("Issues found");

        // Print hlint output
        if !cmd_output.stdout.is_empty() {
            eprintln!("{}", cmd_output.stdout);
        }

        if !fix {
            output.info("Run `hx lint --fix` to apply automatic fixes");
        }

        // hlint returns non-zero if it finds issues, but that's not an error
        Ok(0)
    }
}
