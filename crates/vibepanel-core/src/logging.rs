//! Logging setup using tracing.
//!
//! Provides a simple initialization function for the tracing subscriber
//! with configurable verbosity levels.

use tracing::Level;
use tracing_subscriber::{EnvFilter, fmt};

/// Initialize the global tracing subscriber.
///
/// # Arguments
/// * `verbosity` - Number of `-v` flags passed (0=warn, 1=info, 2=debug, 3+=trace)
///
/// # Example
/// ```
/// use vibepanel_core::logging::init;
/// init(1); // info level
/// ```
pub fn init(verbosity: u8) {
    let level = match verbosity {
        0 => Level::WARN,
        1 => Level::INFO,
        2 => Level::DEBUG,
        _ => Level::TRACE,
    };

    let filter = EnvFilter::from_default_env().add_directive(level.into());

    fmt()
        .with_env_filter(filter)
        .with_target(true)
        .with_thread_ids(false)
        .with_file(false)
        .with_line_number(false)
        .init();
}
