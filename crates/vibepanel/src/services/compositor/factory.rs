//! Backend factory and detection.
//!
//! Provides automatic compositor detection and backend instantiation.

use std::env;
use tracing::{debug, info};

use super::{CompositorBackend, HyprlandBackend, MangoBackend, NiriBackend};

/// Backend kind enum for configuration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendKind {
    /// MangoWC / DWL (uses dwl-ipc-unstable-v2 or mmsg fallback).
    MangoDwl,
    /// Hyprland compositor.
    Hyprland,
    /// Niri compositor.
    Niri,
    /// Auto-detect from environment.
    Auto,
}

impl BackendKind {
    /// Parse a backend kind from a string (case-insensitive).
    #[allow(dead_code)] // Used by tests and for config parsing
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "mango" | "mangowc" | "dwl" => BackendKind::MangoDwl,
            "hyprland" => BackendKind::Hyprland,
            "niri" => BackendKind::Niri,
            "auto" | "" => BackendKind::Auto,
            _ => BackendKind::Auto, // Unknown defaults to auto-detect
        }
    }
}

/// Detect the compositor backend from environment variables.
///
/// Detection order:
/// 1. HYPRLAND_INSTANCE_SIGNATURE → Hyprland
/// 2. NIRI_SOCKET → Niri
/// 3. Default → MangoWC/DWL
pub fn detect_backend() -> BackendKind {
    // Check for Hyprland
    if env::var("HYPRLAND_INSTANCE_SIGNATURE").is_ok() {
        debug!("Detected Hyprland via HYPRLAND_INSTANCE_SIGNATURE");
        return BackendKind::Hyprland;
    }

    // Check for Niri
    if env::var("NIRI_SOCKET").is_ok() {
        debug!("Detected Niri via NIRI_SOCKET");
        return BackendKind::Niri;
    }

    // Default to MangoWC/DWL
    debug!("No specific compositor detected, defaulting to MangoWC/DWL");
    BackendKind::MangoDwl
}

/// Create a compositor backend based on kind and config.
///
/// # Arguments
///
/// * `kind` - The backend kind to create (or Auto for detection).
/// * `outputs` - Optional output allow-list for filtering events.
///
/// # Returns
///
/// A boxed backend implementation ready for use.
pub fn create_backend(
    kind: BackendKind,
    outputs: Option<Vec<String>>,
) -> Box<dyn CompositorBackend> {
    let resolved_kind = if kind == BackendKind::Auto {
        detect_backend()
    } else {
        kind
    };

    info!("Creating compositor backend: {:?}", resolved_kind);

    match resolved_kind {
        BackendKind::MangoDwl => Box::new(MangoBackend::new(outputs)),
        BackendKind::Hyprland => Box::new(HyprlandBackend::new(outputs)),
        BackendKind::Niri => Box::new(NiriBackend::new(outputs)),
        BackendKind::Auto => {
            // Should never reach here after resolution, but handle gracefully
            Box::new(MangoBackend::new(outputs))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_kind_from_str() {
        assert_eq!(BackendKind::from_str("mango"), BackendKind::MangoDwl);
        assert_eq!(BackendKind::from_str("dwl"), BackendKind::MangoDwl);
        assert_eq!(BackendKind::from_str("MangoWC"), BackendKind::MangoDwl);
        assert_eq!(BackendKind::from_str("hyprland"), BackendKind::Hyprland);
        assert_eq!(BackendKind::from_str("HYPRLAND"), BackendKind::Hyprland);
        assert_eq!(BackendKind::from_str("niri"), BackendKind::Niri);
        assert_eq!(BackendKind::from_str("Niri"), BackendKind::Niri);
        assert_eq!(BackendKind::from_str("auto"), BackendKind::Auto);
        assert_eq!(BackendKind::from_str(""), BackendKind::Auto);
        assert_eq!(BackendKind::from_str("unknown"), BackendKind::Auto);
    }
}
