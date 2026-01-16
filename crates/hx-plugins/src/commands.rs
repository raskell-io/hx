//! Custom command support for plugins.

/// A custom command registered by a plugin.
#[derive(Debug, Clone)]
pub struct CustomCommand {
    /// Command name (what users type after `hx`).
    pub name: String,

    /// Description shown in help.
    pub description: String,

    /// Usage string for help.
    pub usage: Option<String>,

    /// Source plugin that registered this command.
    pub source_plugin: Option<String>,
}

impl CustomCommand {
    /// Create a new custom command.
    pub fn new(name: impl Into<String>) -> Self {
        CustomCommand {
            name: name.into(),
            description: String::new(),
            usage: None,
            source_plugin: None,
        }
    }

    /// Set the description.
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Set the usage string.
    pub fn with_usage(mut self, usage: impl Into<String>) -> Self {
        self.usage = Some(usage.into());
        self
    }

    /// Set the source plugin.
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source_plugin = Some(source.into());
        self
    }

    /// Generate help text for this command.
    pub fn help_text(&self) -> String {
        let mut text = String::new();

        if !self.description.is_empty() {
            text.push_str(&self.description);
            text.push('\n');
        }

        if let Some(ref usage) = self.usage {
            text.push('\n');
            text.push_str("Usage: ");
            text.push_str(usage);
            text.push('\n');
        }

        if let Some(ref source) = self.source_plugin {
            text.push('\n');
            text.push_str("(Defined by: ");
            text.push_str(source);
            text.push(')');
        }

        text
    }
}

/// Builder for creating custom commands programmatically.
pub struct CommandBuilder {
    command: CustomCommand,
}

impl CommandBuilder {
    /// Start building a new command.
    pub fn new(name: impl Into<String>) -> Self {
        CommandBuilder {
            command: CustomCommand::new(name),
        }
    }

    /// Set the description.
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.command.description = desc.into();
        self
    }

    /// Set the usage string.
    pub fn usage(mut self, usage: impl Into<String>) -> Self {
        self.command.usage = Some(usage.into());
        self
    }

    /// Set the source plugin.
    pub fn source(mut self, source: impl Into<String>) -> Self {
        self.command.source_plugin = Some(source.into());
        self
    }

    /// Build the command.
    pub fn build(self) -> CustomCommand {
        self.command
    }
}
