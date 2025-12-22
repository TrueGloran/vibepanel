//! VPN card for Quick Settings panel.
//!
//! This module contains:
//! - VPN icon helpers (merged from qs_vpn_helpers.rs)
//! - VPN details panel building
//! - Connection list population
//! - Connection action handling

use std::rc::Rc;

use gtk4::prelude::*;
use gtk4::{Box as GtkBox, GestureClick, ListBox, Orientation, ScrolledWindow};

use super::components::ListRow;
use super::ui_helpers::{
    ExpandableCard, ExpandableCardBase, add_placeholder_row, clear_list_box, create_qs_list_box,
    create_row_action_label, set_icon_active, set_subtitle_active,
};
use crate::services::icons::IconsService;
use crate::services::surfaces::SurfaceStyleManager;
use crate::services::vpn::{VpnConnection, VpnService, VpnSnapshot};
use crate::styles::{color, icon, qs, row};

/// Return an icon name for VPN state.
///
/// Uses standard GTK/Adwaita icon names.
pub fn vpn_icon_name(_any_active: bool) -> &'static str {
    // Always returns "network-vpn" - some themes have state variants but
    // they're not widely supported.
    "network-vpn"
}

/// State for the VPN card in the Quick Settings panel.
///
/// Uses `ExpandableCardBase` for common expandable card fields.
/// VPN has no additional card-specific state beyond the base.
pub struct VpnCardState {
    /// Common expandable card state (toggle, icon, subtitle, list_box, revealer, arrow).
    pub base: ExpandableCardBase,
}

impl VpnCardState {
    pub fn new() -> Self {
        Self {
            base: ExpandableCardBase::new(),
        }
    }
}

impl Default for VpnCardState {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpandableCard for VpnCardState {
    fn base(&self) -> &ExpandableCardBase {
        &self.base
    }
}

/// Result of building VPN details section.
pub struct VpnDetailsResult {
    pub container: GtkBox,
    pub list_box: ListBox,
}

/// Build the VPN details section with connection list.
pub fn build_vpn_details(state: &Rc<VpnCardState>) -> VpnDetailsResult {
    let container = GtkBox::new(Orientation::Vertical, 0);

    // Small top margin for visual spacing
    container.set_margin_top(6);

    // VPN connection list (no scan button needed)
    let list_box = create_qs_list_box();

    let scroller = ScrolledWindow::new();
    scroller.set_policy(gtk4::PolicyType::Never, gtk4::PolicyType::Automatic);
    scroller.set_child(Some(&list_box));
    scroller.set_max_content_height(360);
    scroller.set_propagate_natural_height(true);

    container.append(&scroller);

    // Populate with current VPN state
    let snapshot = VpnService::global().snapshot();
    populate_vpn_list(state, &list_box, &snapshot);

    VpnDetailsResult {
        container,
        list_box,
    }
}

/// Populate the VPN list with connection data from snapshot.
pub fn populate_vpn_list(_state: &VpnCardState, list_box: &ListBox, snapshot: &VpnSnapshot) {
    clear_list_box(list_box);

    if !snapshot.is_ready {
        add_placeholder_row(list_box, "Loading VPN state...");
        return;
    }

    if snapshot.connections.is_empty() {
        add_placeholder_row(list_box, "No VPN connections");
        return;
    }

    let icons = IconsService::global();

    for conn in &snapshot.connections {
        let mut subtitle_parts = Vec::new();
        if conn.active {
            subtitle_parts.push("Active".to_string());
        } else {
            subtitle_parts.push("Inactive".to_string());
        }
        if conn.autoconnect {
            subtitle_parts.push("Autoconnect".to_string());
        }
        // Show VPN type
        if conn.vpn_type == "wireguard" {
            subtitle_parts.push("WireGuard".to_string());
        } else if conn.vpn_type == "vpn" {
            subtitle_parts.push("OpenVPN".to_string());
        }
        let subtitle = subtitle_parts.join(" \u{2022} ");

        let icon_handle =
            icons.create_icon("network-vpn", &[icon::TEXT, row::QS_ICON, color::PRIMARY]);
        let leading_icon = icon_handle.widget();

        let right_widget = create_vpn_action_widget(conn);

        let row_result = ListRow::builder()
            .title(&conn.name)
            .subtitle(&subtitle)
            .leading_widget(leading_icon)
            .trailing_widget(right_widget)
            .css_class(qs::VPN_ROW)
            .build();

        {
            let uuid = conn.uuid.clone();
            row_result.row.connect_activate(move |_| {
                let vpn = VpnService::global();
                vpn.toggle_connection(&uuid);
            });
        }

        list_box.append(&row_result.row);
    }
}

/// Create the action widget for a VPN connection row.
fn create_vpn_action_widget(conn: &VpnConnection) -> gtk4::Widget {
    let uuid = conn.uuid.clone();
    let is_active = conn.active;

    // Single action: "Disconnect" or "Connect" as accent-colored text
    let action_text = if is_active { "Disconnect" } else { "Connect" };
    let action_label = create_row_action_label(action_text);

    let gesture = GestureClick::new();
    gesture.set_button(1);
    gesture.connect_pressed(move |gesture, _, _, _| {
        // Stop propagation to prevent row activation
        gesture.set_state(gtk4::EventSequenceState::Claimed);
        let vpn = VpnService::global();
        vpn.set_connection_state(&uuid, !is_active);
    });
    action_label.add_controller(gesture);

    action_label.upcast()
}

/// Handle VPN state changes from VpnService.
pub fn on_vpn_changed(state: &VpnCardState, snapshot: &VpnSnapshot) {
    let primary = snapshot.primary();
    let has_connections = !snapshot.connections.is_empty();

    // Update toggle state and sensitivity
    if let Some(toggle) = state.base.toggle.borrow().as_ref() {
        let should_be_active = primary.map(|p| p.active).unwrap_or(false);
        if toggle.is_active() != should_be_active {
            toggle.set_active(should_be_active);
        }
        toggle.set_sensitive(has_connections);
    }

    // Update VPN card icon and its active state class
    if let Some(icon_handle) = state.base.card_icon.borrow().as_ref() {
        let icon_name = vpn_icon_name(snapshot.any_active);
        icon_handle.set_icon(icon_name);
        set_icon_active(icon_handle, snapshot.any_active);
    }

    // Update VPN subtitle
    if let Some(label) = state.base.subtitle.borrow().as_ref() {
        let subtitle = if !snapshot.is_ready {
            "VPN".to_string()
        } else if let Some(p) = primary {
            if p.active {
                p.name.clone()
            } else {
                "Disconnected".to_string()
            }
        } else {
            "No connections".to_string()
        };
        label.set_label(&subtitle);
        set_subtitle_active(label, snapshot.any_active);
    }

    // Update connection list
    if let Some(list_box) = state.base.list_box.borrow().as_ref() {
        populate_vpn_list(state, list_box, snapshot);
        // Apply Pango font attrs to dynamically created list rows
        SurfaceStyleManager::global().apply_pango_attrs_all(list_box);
    }
}
