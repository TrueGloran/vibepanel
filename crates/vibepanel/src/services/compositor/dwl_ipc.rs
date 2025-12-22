//! Wayland protocol bindings for DWL IPC v2.
//!
//! This module provides Rust bindings for the `zdwl_ipc_manager_v2` and
//! `zdwl_ipc_output_v2` Wayland protocol interfaces used by MangoWC and DWL.
//!
//! The bindings are generated from the protocol XML file at compile time.

#![allow(dead_code, non_camel_case_types, unused_unsafe, unused_variables)]
#![allow(non_upper_case_globals, non_snake_case, unused_imports)]
#![allow(missing_docs, clippy::all)]

use wayland_client;
use wayland_client::protocol::*;

pub mod __interfaces {
    use wayland_client::protocol::__interfaces::*;
    wayland_scanner::generate_interfaces!("protocols/dwl-ipc-unstable-v2.xml");
}

use self::__interfaces::*;

wayland_scanner::generate_client_code!("protocols/dwl-ipc-unstable-v2.xml");

// Re-export the protocol types with convenient names
pub use zdwl_ipc_manager_v2::ZdwlIpcManagerV2;
pub use zdwl_ipc_output_v2::{TagState, ZdwlIpcOutputV2};
