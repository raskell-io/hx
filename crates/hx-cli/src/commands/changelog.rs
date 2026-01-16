//! Changelog generation command implementation.

use anyhow::{Context, Result};
use hx_config::find_project_root;
use hx_core::CommandRunner;
use hx_ui::Output;
use std::collections::HashMap;
use std::fs;

/// Run the changelog command.
pub async fn run(
    unreleased: bool,
    output_file: Option<String>,
    all: bool,
    preview: bool,
    output: &Output,
) -> Result<i32> {
    let project_root = find_project_root(".")?;
    let runner = CommandRunner::new().with_working_dir(&project_root);

    output.status("Generating", "changelog from git history");

    // Get the last tag
    let last_tag = get_last_tag(&runner).await;

    // Get commits
    let commits = if unreleased {
        get_commits_since_tag(&runner, last_tag.as_deref()).await?
    } else {
        get_all_tagged_commits(&runner).await?
    };

    if commits.is_empty() {
        output.warn("No commits found");
        return Ok(0);
    }

    // Parse and categorize commits
    let changelog = if all {
        generate_changelog_all(&commits, unreleased, last_tag.as_deref())
    } else {
        generate_changelog_conventional(&commits, unreleased, last_tag.as_deref())
    };

    if preview {
        output.header("Changelog Preview");
        println!("{}", changelog);
        return Ok(0);
    }

    // Write to file
    let output_path = output_file.unwrap_or_else(|| "CHANGELOG.md".to_string());
    let full_path = project_root.join(&output_path);

    // If file exists, merge with existing content
    let final_content = if full_path.exists() && unreleased {
        merge_with_existing(&full_path, &changelog)?
    } else {
        changelog
    };

    fs::write(&full_path, &final_content)
        .with_context(|| format!("Failed to write {}", output_path))?;

    output.status("Generated", &output_path);

    if unreleased {
        if let Some(tag) = &last_tag {
            output.info(&format!("Changes since {}", tag));
        } else {
            output.info("All unreleased changes");
        }
    }

    Ok(0)
}

/// Get the last git tag.
async fn get_last_tag(runner: &CommandRunner) -> Option<String> {
    let output = runner
        .run("git", ["describe", "--tags", "--abbrev=0"])
        .await
        .ok()?;

    if output.success() {
        Some(output.stdout.trim().to_string())
    } else {
        None
    }
}

/// Get commits since a specific tag.
async fn get_commits_since_tag(
    runner: &CommandRunner,
    tag: Option<&str>,
) -> Result<Vec<Commit>> {
    let range = match tag {
        Some(t) => format!("{}..HEAD", t),
        None => "HEAD".to_string(),
    };

    let output = runner
        .run(
            "git",
            [
                "log",
                &range,
                "--pretty=format:%H|%s|%an|%ad",
                "--date=short",
            ],
        )
        .await?;

    if !output.success() {
        return Ok(vec![]);
    }

    Ok(parse_commits(&output.stdout))
}

/// Get all commits grouped by tags.
async fn get_all_tagged_commits(runner: &CommandRunner) -> Result<Vec<Commit>> {
    // Get all commits
    let output = runner
        .run(
            "git",
            ["log", "--pretty=format:%H|%s|%an|%ad", "--date=short"],
        )
        .await?;

    if !output.success() {
        return Ok(vec![]);
    }

    Ok(parse_commits(&output.stdout))
}

/// Parse git log output into commits.
fn parse_commits(output: &str) -> Vec<Commit> {
    output
        .lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.splitn(4, '|').collect();
            if parts.len() >= 4 {
                Some(Commit {
                    hash: parts[0].to_string(),
                    message: parts[1].to_string(),
                    author: parts[2].to_string(),
                    date: parts[3].to_string(),
                })
            } else {
                None
            }
        })
        .collect()
}

/// A git commit.
#[derive(Debug)]
#[allow(dead_code)]
struct Commit {
    hash: String,
    message: String,
    author: String,
    date: String,
}

/// Conventional commit type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum CommitType {
    Feature,
    Fix,
    Docs,
    Style,
    Refactor,
    Perf,
    Test,
    Build,
    Ci,
    Chore,
    Breaking,
    Other,
}

impl CommitType {
    fn from_prefix(prefix: &str) -> Self {
        match prefix.to_lowercase().as_str() {
            "feat" | "feature" => Self::Feature,
            "fix" | "bugfix" => Self::Fix,
            "docs" | "doc" => Self::Docs,
            "style" => Self::Style,
            "refactor" => Self::Refactor,
            "perf" | "performance" => Self::Perf,
            "test" | "tests" => Self::Test,
            "build" => Self::Build,
            "ci" => Self::Ci,
            "chore" => Self::Chore,
            _ => Self::Other,
        }
    }

    fn header(&self) -> &'static str {
        match self {
            Self::Feature => "### Features",
            Self::Fix => "### Bug Fixes",
            Self::Docs => "### Documentation",
            Self::Style => "### Styles",
            Self::Refactor => "### Refactoring",
            Self::Perf => "### Performance",
            Self::Test => "### Tests",
            Self::Build => "### Build",
            Self::Ci => "### CI",
            Self::Chore => "### Chores",
            Self::Breaking => "### Breaking Changes",
            Self::Other => "### Other",
        }
    }

    fn order(&self) -> u8 {
        match self {
            Self::Breaking => 0,
            Self::Feature => 1,
            Self::Fix => 2,
            Self::Perf => 3,
            Self::Refactor => 4,
            Self::Docs => 5,
            Self::Test => 6,
            Self::Build => 7,
            Self::Ci => 8,
            Self::Style => 9,
            Self::Chore => 10,
            Self::Other => 11,
        }
    }
}

/// Parse a conventional commit message.
fn parse_conventional_commit(message: &str) -> (CommitType, String, bool) {
    // Pattern: type(scope)!: description or type!: description or type: description
    let is_breaking = message.contains('!') && message.contains(':');

    if let Some(colon_pos) = message.find(':') {
        let prefix = &message[..colon_pos];
        let description = message[colon_pos + 1..].trim();

        // Extract type (removing scope and !)
        let type_str = prefix
            .split('(')
            .next()
            .unwrap_or(prefix)
            .trim_end_matches('!');

        let commit_type = CommitType::from_prefix(type_str);

        (commit_type, description.to_string(), is_breaking)
    } else {
        (CommitType::Other, message.to_string(), false)
    }
}

/// Generate changelog using conventional commits format.
fn generate_changelog_conventional(
    commits: &[Commit],
    unreleased: bool,
    last_tag: Option<&str>,
) -> String {
    let mut categorized: HashMap<CommitType, Vec<&Commit>> = HashMap::new();
    let mut breaking: Vec<&Commit> = Vec::new();

    for commit in commits {
        let (commit_type, _, is_breaking) = parse_conventional_commit(&commit.message);

        if is_breaking {
            breaking.push(commit);
        }

        categorized.entry(commit_type).or_default().push(commit);
    }

    let mut output = String::new();

    // Header
    output.push_str("# Changelog\n\n");
    output.push_str("All notable changes to this project will be documented in this file.\n\n");
    output.push_str("The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),\n");
    output.push_str("and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).\n\n");

    // Version header
    if unreleased {
        output.push_str("## [Unreleased]\n\n");
    } else if let Some(tag) = last_tag {
        let date = commits.first().map(|c| c.date.as_str()).unwrap_or("today");
        output.push_str(&format!("## [{}] - {}\n\n", tag, date));
    }

    // Breaking changes first
    if !breaking.is_empty() {
        output.push_str("### Breaking Changes\n\n");
        for commit in &breaking {
            let (_, description, _) = parse_conventional_commit(&commit.message);
            output.push_str(&format!("- {} ({})\n", description, &commit.hash[..7]));
        }
        output.push('\n');
    }

    // Sort categories by order
    let mut sorted_categories: Vec<_> = categorized.into_iter().collect();
    sorted_categories.sort_by_key(|(t, _)| t.order());

    for (commit_type, commits) in sorted_categories {
        if commit_type == CommitType::Other && commits.is_empty() {
            continue;
        }

        output.push_str(commit_type.header());
        output.push_str("\n\n");

        for commit in commits {
            let (_, description, _) = parse_conventional_commit(&commit.message);
            output.push_str(&format!("- {} ({})\n", description, &commit.hash[..7]));
        }
        output.push('\n');
    }

    output
}

/// Generate changelog including all commits.
fn generate_changelog_all(
    commits: &[Commit],
    unreleased: bool,
    last_tag: Option<&str>,
) -> String {
    let mut output = String::new();

    // Header
    output.push_str("# Changelog\n\n");

    // Version header
    if unreleased {
        output.push_str("## [Unreleased]\n\n");
    } else if let Some(tag) = last_tag {
        let date = commits.first().map(|c| c.date.as_str()).unwrap_or("today");
        output.push_str(&format!("## [{}] - {}\n\n", tag, date));
    }

    // All commits
    for commit in commits {
        output.push_str(&format!(
            "- {} ({}) - {}\n",
            commit.message,
            &commit.hash[..7],
            commit.date
        ));
    }

    output
}

/// Merge new unreleased changes with existing changelog.
fn merge_with_existing(path: &std::path::Path, new_content: &str) -> Result<String> {
    let existing = fs::read_to_string(path)?;

    // Find where the existing changelog content starts (after header)
    // and insert new unreleased section
    if let Some(pos) = existing.find("## [") {
        // Extract the new unreleased section (skip the header parts)
        let new_unreleased = new_content
            .find("## [Unreleased]")
            .map(|start| {
                let end = new_content[start + 15..]
                    .find("## [")
                    .map(|e| start + 15 + e)
                    .unwrap_or(new_content.len());
                &new_content[start..end]
            })
            .unwrap_or("");

        if new_unreleased.is_empty() {
            return Ok(existing);
        }

        // Check if there's already an unreleased section
        if existing.contains("## [Unreleased]") {
            // Replace existing unreleased section
            if let Some(unreleased_start) = existing.find("## [Unreleased]") {
                let unreleased_end = existing[unreleased_start + 15..]
                    .find("## [")
                    .map(|e| unreleased_start + 15 + e)
                    .unwrap_or(existing.len());

                let mut result = existing[..unreleased_start].to_string();
                result.push_str(new_unreleased);
                result.push_str(&existing[unreleased_end..]);
                return Ok(result);
            }
        }

        // Insert new unreleased section before first version
        let mut result = existing[..pos].to_string();
        result.push_str(new_unreleased);
        result.push('\n');
        result.push_str(&existing[pos..]);
        Ok(result)
    } else {
        // No existing versions, just use new content
        Ok(new_content.to_string())
    }
}
