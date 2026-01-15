//! Project initialization command.

use anyhow::Result;
use hx_config::{Manifest, ProjectKind, MANIFEST_FILENAME};
use hx_ui::{Output, Spinner};
use std::fs;
use std::path::PathBuf;

/// Run the init command.
pub async fn run(
    _bin: bool,
    lib: bool,
    name: Option<String>,
    dir: Option<String>,
    output: &Output,
) -> Result<i32> {
    let spinner = Spinner::new("Initializing project...");

    // Determine project directory
    let project_dir = if let Some(d) = dir {
        PathBuf::from(d)
    } else {
        std::env::current_dir()?
    };

    // Determine project name
    let project_name = name.unwrap_or_else(|| {
        project_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("myproject")
            .to_string()
    });

    // Determine project kind
    let kind = if lib {
        ProjectKind::Lib
    } else {
        ProjectKind::Bin
    };

    // Create project directory if needed
    if !project_dir.exists() {
        fs::create_dir_all(&project_dir)?;
    }

    // Check if hx.toml already exists
    let manifest_path = project_dir.join(MANIFEST_FILENAME);
    if manifest_path.exists() {
        spinner.finish_warning("Project already initialized");
        output.warn(&format!("{} already exists", MANIFEST_FILENAME));
        return Ok(1);
    }

    // Create hx.toml
    let manifest = Manifest::new(&project_name, kind);
    manifest.to_file(&manifest_path)?;

    // Create .gitignore
    let gitignore_path = project_dir.join(".gitignore");
    if !gitignore_path.exists() {
        fs::write(
            &gitignore_path,
            r#"# hx
.hx/
dist-newstyle/

# Cabal
*.hi
*.o
*.dyn_hi
*.dyn_o
"#,
        )?;
    }

    // Create src directory
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir)?;

    // Create main source file
    match kind {
        ProjectKind::Bin => {
            let main_path = src_dir.join("Main.hs");
            if !main_path.exists() {
                fs::write(
                    &main_path,
                    r#"module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
"#,
                )?;
            }
        }
        ProjectKind::Lib => {
            let lib_path = src_dir.join("Lib.hs");
            if !lib_path.exists() {
                fs::write(
                    &lib_path,
                    format!(
                        r#"module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "{}"
"#,
                        project_name
                    ),
                )?;
            }
        }
    }

    // Create .cabal file
    let cabal_path = project_dir.join(format!("{}.cabal", project_name));
    if !cabal_path.exists() {
        let cabal_content = generate_cabal(&project_name, kind);
        fs::write(&cabal_path, cabal_content)?;
    }

    // Create .editorconfig
    let editorconfig_path = project_dir.join(".editorconfig");
    if !editorconfig_path.exists() {
        fs::write(
            &editorconfig_path,
            r#"root = true

[*]
indent_style = space
indent_size = 2
end_of_line = lf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true

[*.hs]
indent_size = 2

[*.cabal]
indent_size = 2

[Makefile]
indent_style = tab
"#,
        )?;
    }

    spinner.finish_success(format!("Created {} project: {}", kind.as_str(), project_name));

    output.info("");
    output.info("Next steps:");
    output.info(&format!("  cd {}", project_dir.display()));
    output.info("  hx build");
    if kind == ProjectKind::Bin {
        output.info("  hx run");
    }

    Ok(0)
}

fn generate_cabal(name: &str, kind: ProjectKind) -> String {
    match kind {
        ProjectKind::Bin => format!(
            r#"cabal-version:      3.0
name:               {name}
version:            0.1.0.0
synopsis:           A Haskell project
license:            MIT
author:             Author
maintainer:         author@example.com
build-type:         Simple

executable {name}
    main-is:          Main.hs
    hs-source-dirs:   src
    default-language: GHC2021
    build-depends:
        base ^>=4.17 || ^>=4.18 || ^>=4.19 || ^>=4.20
    ghc-options:      -Wall
"#
        ),
        ProjectKind::Lib => format!(
            r#"cabal-version:      3.0
name:               {name}
version:            0.1.0.0
synopsis:           A Haskell library
license:            MIT
author:             Author
maintainer:         author@example.com
build-type:         Simple

library
    exposed-modules:  Lib
    hs-source-dirs:   src
    default-language: GHC2021
    build-depends:
        base ^>=4.17 || ^>=4.18 || ^>=4.19 || ^>=4.20
    ghc-options:      -Wall
"#
        ),
    }
}
