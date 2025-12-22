//! Shared runtime services for the vibepanel bar.
//!
//! This module provides long-lived, process-wide services that can be
//! shared across multiple widgets and windows (e.g. multi-monitor bars).
//!
//! ## Services
//!
//! - **battery**: UPower-backed battery state monitoring
//! - **config_manager**: Configuration hot-reload with file watching
//! - **icons**: Icon theme management (Material Symbols font, icon name mapping)
//! - **tooltip**: Styled GTK tooltips
//! - **surfaces**: Shared surface styling for popovers, menus, overlays
//! - **compositor**: Pluggable compositor backend abstraction
//! - **workspace**: Workspace state monitoring
//! - **window_title**: Focused window title monitoring
//! - **system_tray**: StatusNotifierItem host for system tray icons
//! - **vpn**: VPN connection management via NetworkManager
//! - **idle_inhibitor**: System idle/sleep prevention
//! - **state**: Persistent state storage (DND, VPN last used, notification history)
//! - **system**: CPU, memory, and system resource monitoring

pub mod audio;
pub mod bar_manager;
pub mod battery;
pub mod bluetooth;
pub mod brightness;
pub mod callbacks;
pub mod compositor;
pub mod config_manager;
pub mod icons;
pub mod idle_inhibitor;
pub mod network;
pub mod notification;
pub mod osd_ipc;
pub mod power_profile;
pub mod state;
pub mod surfaces;
pub mod system;
pub mod system_tray;
pub mod tooltip;
pub mod updates;
pub mod vpn;
pub mod window_title;
pub mod workspace;
