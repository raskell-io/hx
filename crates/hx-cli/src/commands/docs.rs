//! Documentation generation command implementation.

use anyhow::{Context, Result};
use hx_config::{Project, find_project_root};
use hx_core::CommandRunner;
use hx_ui::{Output, Spinner};
use std::path::PathBuf;

/// Run the docs command.
pub async fn run(open: bool, deps: bool, serve: bool, port: u16, output: &Output) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let project = Project::load(&project_root)?;

    output.status("Documenting", project.name());

    let runner = CommandRunner::new().with_working_dir(&project_root);

    // Build documentation with haddock
    let spinner = Spinner::new("Building documentation...");

    let mut haddock_args = vec!["haddock"];

    if deps {
        haddock_args.push("--haddock-all");
    }

    // Add hyperlinks
    haddock_args.push("--haddock-hyperlink-source");

    let haddock_output = runner
        .run("cabal", haddock_args.iter().copied())
        .await?;

    if !haddock_output.success() {
        spinner.finish_error("Documentation build failed");
        eprintln!("{}", haddock_output.stderr);
        return Ok(1);
    }

    spinner.finish_success("Documentation built");

    // Find the generated documentation path
    let doc_path = find_doc_path(&project_root, project.name())?;

    if serve {
        output.status("Serving", &format!("documentation at http://localhost:{}", port));
        output.info(&format!("Documentation: {}", doc_path.display()));
        output.info("Press Ctrl+C to stop the server");

        // Start a simple HTTP server
        serve_docs(&doc_path, port).await?;
    } else if open {
        // Open in browser
        let index_path = doc_path.join("index.html");
        if index_path.exists() {
            output.status("Opening", &index_path.display().to_string());
            open_in_browser(&index_path)?;
        } else {
            output.warn(&format!("Documentation index not found at {}", index_path.display()));
            output.info(&format!("Documentation directory: {}", doc_path.display()));
        }
    } else {
        output.status("Generated", &format!("documentation in {}", doc_path.display()));
        output.info("Use --open to view in browser, or --serve to start a local server");
    }

    Ok(0)
}

/// Find the generated documentation path.
fn find_doc_path(project_root: &std::path::Path, package_name: &str) -> Result<PathBuf> {
    // Try common haddock output locations
    let candidates = [
        // Standard cabal v2 location
        project_root
            .join(".hx")
            .join("cabal")
            .join("dist-newstyle")
            .join("build")
            .join("*")
            .join("ghc-*")
            .join(package_name)
            .join("doc")
            .join("html")
            .join(package_name),
        // Fallback: look in dist-newstyle directly
        project_root.join("dist-newstyle").join("build"),
    ];

    // Try to find the actual path using glob
    for candidate in &candidates {
        let pattern = candidate.to_string_lossy();
        if let Ok(paths) = glob::glob(&pattern) {
            for path in paths.flatten() {
                if path.exists() && path.is_dir() {
                    return Ok(path);
                }
            }
        }
    }

    // Try a more exhaustive search
    let doc_base = project_root
        .join(".hx")
        .join("cabal")
        .join("dist-newstyle");

    if doc_base.exists() {
        // Search recursively for the package's doc directory
        if let Some(path) = find_doc_dir_recursive(&doc_base, package_name) {
            return Ok(path);
        }
    }

    // Fallback to dist-newstyle if .hx doesn't exist
    let fallback = project_root.join("dist-newstyle");
    if fallback.exists()
        && let Some(path) = find_doc_dir_recursive(&fallback, package_name)
    {
        return Ok(path);
    }

    // Return default expected path even if it doesn't exist
    Ok(project_root
        .join(".hx")
        .join("cabal")
        .join("dist-newstyle")
        .join("doc")
        .join("html")
        .join(package_name))
}

/// Recursively search for documentation directory.
fn find_doc_dir_recursive(base: &std::path::Path, package_name: &str) -> Option<PathBuf> {
    let mut stack = vec![base.to_path_buf()];

    while let Some(dir) = stack.pop() {
        if let Ok(entries) = std::fs::read_dir(&dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    // Check if this is the package doc directory
                    if path.ends_with(format!("html/{}", package_name)) {
                        return Some(path);
                    }
                    // Check if this directory contains index.html and looks like docs
                    if path.file_name().map(|n| n == package_name).unwrap_or(false)
                        && path.join("index.html").exists()
                    {
                        return Some(path);
                    }
                    stack.push(path);
                }
            }
        }
    }

    None
}

/// Open a file in the default browser.
fn open_in_browser(path: &std::path::Path) -> Result<()> {
    #[cfg(target_os = "macos")]
    {
        std::process::Command::new("open")
            .arg(path)
            .spawn()
            .context("Failed to open browser")?;
    }

    #[cfg(target_os = "linux")]
    {
        std::process::Command::new("xdg-open")
            .arg(path)
            .spawn()
            .context("Failed to open browser")?;
    }

    #[cfg(target_os = "windows")]
    {
        std::process::Command::new("cmd")
            .args(["/C", "start", "", &path.display().to_string()])
            .spawn()
            .context("Failed to open browser")?;
    }

    Ok(())
}

/// Serve documentation using a simple HTTP server.
async fn serve_docs(doc_path: &std::path::Path, port: u16) -> Result<()> {
    use std::io::{Read, Write};
    use std::net::TcpListener;

    let listener = TcpListener::bind(format!("127.0.0.1:{}", port))
        .with_context(|| format!("Failed to bind to port {}", port))?;

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                let mut buffer = [0; 4096];
                if stream.read(&mut buffer).is_ok() {
                    let request = String::from_utf8_lossy(&buffer);

                    // Parse the requested path
                    let path = request
                        .lines()
                        .next()
                        .and_then(|line| line.split_whitespace().nth(1))
                        .unwrap_or("/");

                    let file_path = if path == "/" {
                        doc_path.join("index.html")
                    } else {
                        doc_path.join(path.trim_start_matches('/'))
                    };

                    let (status, content_type, body) = if file_path.exists() {
                        let content = std::fs::read(&file_path).unwrap_or_default();
                        let ext = file_path
                            .extension()
                            .and_then(|e| e.to_str())
                            .unwrap_or("");
                        let mime = match ext {
                            "html" => "text/html",
                            "css" => "text/css",
                            "js" => "application/javascript",
                            "png" => "image/png",
                            "gif" => "image/gif",
                            "svg" => "image/svg+xml",
                            _ => "application/octet-stream",
                        };
                        ("200 OK", mime, content)
                    } else {
                        (
                            "404 Not Found",
                            "text/html",
                            b"<h1>404 Not Found</h1>".to_vec(),
                        )
                    };

                    let response = format!(
                        "HTTP/1.1 {}\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n",
                        status,
                        content_type,
                        body.len()
                    );

                    let _ = stream.write_all(response.as_bytes());
                    let _ = stream.write_all(&body);
                }
            }
            Err(e) => {
                eprintln!("Connection error: {}", e);
            }
        }
    }

    Ok(())
}
