//! Error types for vibepanel-core.

use std::path::PathBuf;

/// Result type alias using the crate's Error type.
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur in vibepanel-core.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Configuration file not found.
    #[error("config file not found: {0}")]
    ConfigNotFound(PathBuf),

    /// Failed to read configuration file.
    #[error("failed to read config file: {0}")]
    ConfigRead(#[from] std::io::Error),

    /// Failed to parse TOML configuration.
    #[error("failed to parse config: {0}")]
    ConfigParse(#[from] toml::de::Error),

    /// Configuration validation failed.
    #[error("config validation failed:\n{}", .0.join("\n"))]
    ConfigValidation(Vec<String>),
}
