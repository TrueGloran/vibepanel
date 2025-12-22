//! Wi-Fi card for Quick Settings panel.
//!
//! This module contains:
//! - Wi-Fi icon helpers (merged from qs_wifi_helpers.rs)
//! - Wi-Fi details panel building
//! - Network list population
//! - Password dialog handling
//! - Scan animation

use std::cell::{Cell, RefCell};
use std::rc::{Rc, Weak};

use gtk4::glib::{self, WeakRef};
use gtk4::prelude::*;
use gtk4::{
    ApplicationWindow, Box as GtkBox, Button, Entry, GestureClick, Label, ListBox, ListBoxRow,
    Orientation, Overlay, Popover, ScrolledWindow,
};
use tracing::debug;

use super::components::ListRow;
use super::ui_helpers::{
    ExpandableCard, ExpandableCardBase, add_placeholder_row, build_scan_button, clear_list_box,
    create_qs_list_box, create_row_action_label, create_row_menu_action, create_row_menu_button,
    set_icon_active, set_subtitle_active,
};
use crate::services::icons::IconsService;
use crate::services::network::{NetworkService, NetworkSnapshot, WifiNetwork};
use crate::services::surfaces::SurfaceStyleManager;
use crate::styles::{color, icon, qs, row, state, surface};
use crate::widgets::base::configure_popover;

/// Return a simple connected/disconnected Wi-Fi icon.
///
/// The main card widget uses this for a stable "connected" icon,
/// while the per-network list rows use `wifi_strength_icon` for
/// detailed signal levels.
pub fn wifi_icon_name(connected: bool, wifi_enabled: bool) -> &'static str {
    if !wifi_enabled {
        "network-wireless-offline-symbolic"
    } else if connected {
        "network-wireless-signal-excellent-symbolic"
    } else {
        "network-wireless-offline-symbolic"
    }
}

/// Return a Wi-Fi icon name based on a raw signal strength percentage.
///
/// The list rows use this to express 1/2/3/4-bar states. The Material
/// icon mapping compresses these into the available glyph set.
pub fn wifi_strength_icon(level: i32) -> &'static str {
    if level >= 70 {
        "network-wireless-signal-excellent-symbolic"
    } else if level >= 60 {
        "network-wireless-signal-good-symbolic"
    } else if level >= 40 {
        "network-wireless-signal-ok-symbolic"
    } else if level >= 20 {
        "network-wireless-signal-weak-symbolic"
    } else {
        "network-wireless-signal-none-symbolic"
    }
}

/// State for the Wi-Fi card in the Quick Settings panel.
///
/// Uses `ExpandableCardBase` for common expandable card fields and adds
/// Wi-Fi specific state (scan button, password dialog, animation).
pub struct WifiCardState {
    /// Common expandable card state (toggle, icon, subtitle, list_box, revealer, arrow).
    pub base: ExpandableCardBase,
    /// The Wi-Fi scan button.
    pub scan_button: RefCell<Option<Button>>,
    /// The Wi-Fi scan button label.
    pub scan_label: RefCell<Option<Label>>,
    /// Inline password box.
    pub password_box: RefCell<Option<GtkBox>>,
    /// Label in the password box.
    pub password_label: RefCell<Option<Label>>,
    /// Password entry field.
    pub password_entry: RefCell<Option<Entry>>,
    /// Target SSID for the inline password prompt.
    pub password_target_ssid: RefCell<Option<String>>,
    /// Scan animation GLib source ID.
    pub scan_anim_source: RefCell<Option<glib::SourceId>>,
    /// Scan animation step counter.
    pub scan_anim_step: Cell<u8>,
    /// Flag to prevent toggle signal handler from firing during programmatic updates.
    /// This prevents feedback loops when the service notifies us of state changes.
    pub updating_toggle: Cell<bool>,
}

impl WifiCardState {
    pub fn new() -> Self {
        Self {
            base: ExpandableCardBase::new(),
            scan_button: RefCell::new(None),
            scan_label: RefCell::new(None),
            password_box: RefCell::new(None),
            password_label: RefCell::new(None),
            password_entry: RefCell::new(None),
            password_target_ssid: RefCell::new(None),
            scan_anim_source: RefCell::new(None),
            scan_anim_step: Cell::new(0),
            updating_toggle: Cell::new(false),
        }
    }
}

impl Default for WifiCardState {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpandableCard for WifiCardState {
    fn base(&self) -> &ExpandableCardBase {
        &self.base
    }
}

impl Drop for WifiCardState {
    fn drop(&mut self) {
        // Cancel any active scan animation timer
        if let Some(source_id) = self.scan_anim_source.borrow_mut().take() {
            source_id.remove();
            debug!("WifiCardState: scan animation timer cancelled on drop");
        }
    }
}

/// Result of building Wi-Fi details section.
pub struct WifiDetailsResult {
    pub container: GtkBox,
    pub list_box: ListBox,
    pub scan_button: Button,
    pub scan_label: Label,
}

/// Build the Wi-Fi details section with scan button, network list, and
/// inline password prompt.
pub fn build_wifi_details(
    state: &Rc<WifiCardState>,
    window: WeakRef<ApplicationWindow>,
) -> WifiDetailsResult {
    let container = GtkBox::new(Orientation::Vertical, 0);

    // Scan button
    let scan_result = build_scan_button("Scan");
    let scan_button = scan_result.button;
    let scan_label = scan_result.label;

    {
        scan_button.connect_clicked(move |_| {
            NetworkService::global().scan_networks();
        });
    }

    container.append(&scan_button);

    // Network list
    let list_box = create_qs_list_box();

    let scroller = ScrolledWindow::new();
    scroller.set_policy(gtk4::PolicyType::Never, gtk4::PolicyType::Automatic);
    scroller.set_child(Some(&list_box));
    scroller.set_max_content_height(360);
    scroller.set_propagate_natural_height(true);

    container.append(&scroller);

    // Inline password prompt box (initially hidden, reused as a row child)
    let pwd_box = GtkBox::new(Orientation::Vertical, 6);
    pwd_box.add_css_class(qs::PASSWORD_INLINE);
    pwd_box.set_visible(false);

    let pwd_label = Label::new(Some(""));
    pwd_label.set_xalign(0.0);
    pwd_box.append(&pwd_label);

    let pwd_entry = Entry::new();
    pwd_entry.set_visibility(false);
    pwd_entry.set_input_purpose(gtk4::InputPurpose::Password);
    pwd_entry.set_can_focus(true);
    pwd_entry.set_focus_on_click(true);

    {
        let state_weak = Rc::downgrade(state);
        pwd_entry.connect_map(move |entry| {
            if let Some(state) = state_weak.upgrade() {
                on_password_entry_mapped(&state, entry);
            }
        });
    }
    {
        let state_weak = Rc::downgrade(state);
        let window_weak = window.clone();
        pwd_entry.connect_activate(move |_| {
            if let Some(state) = state_weak.upgrade() {
                on_password_connect_clicked(&state, window_weak.clone());
            }
        });
    }

    pwd_box.append(&pwd_entry);

    let btn_row = GtkBox::new(Orientation::Horizontal, 4);
    btn_row.set_halign(gtk4::Align::End);

    let btn_cancel = Button::with_label("Cancel");
    btn_cancel.add_css_class(qs::PASSWORD_BUTTON);
    let btn_ok = Button::with_label("Connect");
    btn_ok.add_css_class(qs::PASSWORD_BUTTON);

    {
        let state_weak = Rc::downgrade(state);
        let window_weak = window.clone();
        btn_cancel.connect_clicked(move |_| {
            if let Some(state) = state_weak.upgrade() {
                on_password_cancel_clicked(&state, window_weak.clone());
            }
        });
    }

    {
        let state_weak = Rc::downgrade(state);
        let window_weak = window.clone();
        btn_ok.connect_clicked(move |_| {
            if let Some(state) = state_weak.upgrade() {
                on_password_connect_clicked(&state, window_weak.clone());
            }
        });
    }

    btn_row.append(&btn_cancel);
    btn_row.append(&btn_ok);
    pwd_box.append(&btn_row);

    // Store password widgets for later use
    *state.password_box.borrow_mut() = Some(pwd_box.clone());
    *state.password_label.borrow_mut() = Some(pwd_label.clone());
    *state.password_entry.borrow_mut() = Some(pwd_entry.clone());

    // Populate with current network state
    let snapshot = NetworkService::global().snapshot();
    populate_wifi_list(state, &list_box, &snapshot);

    WifiDetailsResult {
        container,
        list_box,
        scan_button,
        scan_label,
    }
}

/// Populate the Wi-Fi list with network data from snapshot.
pub fn populate_wifi_list(state: &WifiCardState, list_box: &ListBox, snapshot: &NetworkSnapshot) {
    clear_list_box(list_box);

    if !snapshot.is_ready {
        add_placeholder_row(list_box, "Scanning for networks...");
        return;
    }

    if snapshot.networks.is_empty() {
        add_placeholder_row(list_box, "No networks found");
        return;
    }

    let icons = IconsService::global();
    let target_ssid = state.password_target_ssid.borrow().clone();
    let connecting_ssid = snapshot.connecting_ssid.clone();
    let mut inserted_password_row = false;

    for net in &snapshot.networks {
        // Check if this network is currently being connected to
        let is_connecting = connecting_ssid.as_ref() == Some(&net.ssid);

        // Build subtitle
        let mut subtitle_parts = Vec::new();
        if is_connecting {
            subtitle_parts.push("Connecting...".to_string());
        } else if net.active {
            subtitle_parts.push("Connected".to_string());
        }
        if net.security != "open" {
            subtitle_parts.push("Secured".to_string());
        }
        if net.known {
            subtitle_parts.push("Saved".to_string());
        }
        subtitle_parts.push(format!("{}%", net.strength));
        let subtitle = subtitle_parts.join(" \u{2022} ");

        // Create signal strength icon
        let strength_icon_name = wifi_strength_icon(net.strength);

        // Check if this is a partial signal that needs the overlay treatment
        let needs_overlay = matches!(
            strength_icon_name,
            "network-wireless-signal-none-symbolic"
                | "network-wireless-signal-weak-symbolic"
                | "network-wireless-signal-ok-symbolic"
                | "network-wireless-signal-good-symbolic"
        );

        let leading_icon: gtk4::Widget = if icons.uses_material() && needs_overlay {
            // Create base icon (full signal, dimmed)
            let base_handle = icons.create_icon(
                "network-wireless-signal-excellent-symbolic",
                &[icon::TEXT, row::QS_ICON, qs::WIFI_BASE, color::MUTED],
            );

            // Create overlay icon (actual signal level, highlighted)
            let overlay_handle = icons.create_icon(
                strength_icon_name,
                &[icon::TEXT, row::QS_ICON, qs::WIFI_OVERLAY, color::PRIMARY],
            );

            // Stack them using Overlay
            let overlay = Overlay::new();
            overlay.set_child(Some(&base_handle.widget()));
            overlay.add_overlay(&overlay_handle.widget());
            overlay.upcast()
        } else {
            // Simple single icon for full signal or non-Material themes
            let icon_handle = icons.create_icon(
                strength_icon_name,
                &[icon::TEXT, row::QS_ICON, color::PRIMARY],
            );
            icon_handle.widget()
        };

        // Create action widget with click handler (or placeholder if connecting)
        let right_widget = if is_connecting {
            // Show a muted "Connecting..." label instead of action button
            let connecting_label = Label::new(Some("..."));
            connecting_label.add_css_class(color::MUTED);
            connecting_label.upcast::<gtk4::Widget>()
        } else {
            create_network_action_widget(net)
        };

        let row_result = ListRow::builder()
            .title(&net.ssid)
            .subtitle(&subtitle)
            .leading_widget(leading_icon)
            .trailing_widget(right_widget)
            .css_class(qs::WIFI_ROW)
            .build();

        // Disable row activation if this network is currently connecting
        if is_connecting {
            row_result.row.set_activatable(false);
            row_result.row.set_sensitive(false);
        }

        // Connect row activation to the primary network action
        if !is_connecting {
            let ssid = net.ssid.clone();
            let security = net.security.clone();
            let known = net.known;
            let active = net.active;
            row_result.row.connect_activate(move |_| {
                let service = NetworkService::global();
                if active {
                    service.disconnect();
                } else if security == "open" || known {
                    service.connect_to_ssid(&ssid, None);
                } else {
                    // Secured, unknown network: show password prompt
                    let snapshot = service.snapshot();
                    if snapshot
                        .networks
                        .iter()
                        .any(|n| n.ssid == ssid && !n.known && n.security != "open")
                    {
                        // Look up the QuickSettingsWindow to show the inline password dialog.
                        if let Some(window) = gtk4::Window::list_toplevels()
                            .into_iter()
                            .find_map(|w| w.downcast::<ApplicationWindow>().ok())
                        {
                            // SAFETY: We store a Weak<QuickSettingsWindow> on the window at creation
                            // time with key "vibepanel-qs-window". upgrade() returns None if dropped.
                            unsafe {
                                if let Some(weak_ptr) =
                                    window.data::<Weak<super::window::QuickSettingsWindow>>(
                                        "vibepanel-qs-window",
                                    )
                                    && let Some(qs) = weak_ptr.as_ref().upgrade()
                                {
                                    qs.show_wifi_password_dialog(&ssid);
                                }
                            }
                        }
                    }
                }
            });
        }

        list_box.append(&row_result.row);

        // Insert password row directly under the matching network row
        if let Some(ref target) = target_ssid
            && !target.is_empty()
            && *target == net.ssid
            && let Some(pwd_box) = state.password_box.borrow().as_ref()
        {
            let pwd_row = ListBoxRow::new();
            pwd_row.add_css_class(qs::PASSWORD_ROW);
            pwd_row.set_activatable(false);
            pwd_row.set_focusable(true);
            pwd_box.set_visible(true);
            pwd_row.set_child(Some(pwd_box));
            list_box.append(&pwd_row);
            inserted_password_row = true;
        }
    }

    // Fallback: append password row at end if target SSID not found
    if let Some(target) = target_ssid
        && !target.is_empty()
        && !inserted_password_row
        && let Some(pwd_box) = state.password_box.borrow().as_ref()
    {
        let pwd_row = ListBoxRow::new();
        pwd_row.add_css_class(qs::PASSWORD_ROW);
        pwd_row.set_activatable(false);
        pwd_row.set_focusable(true);
        pwd_box.set_visible(true);
        pwd_row.set_child(Some(pwd_box));
        list_box.append(&pwd_row);
    }
}

/// Create the action widget for a network row.
fn create_network_action_widget(net: &WifiNetwork) -> gtk4::Widget {
    let ssid = net.ssid.clone();
    let is_active = net.active;
    let is_known = net.known;

    // Determine if we need a menu (multiple actions) or single action label
    let has_multiple_actions = is_active || is_known;

    if !has_multiple_actions {
        // Single action: just "Connect" as accent-colored text
        let action_label = create_row_action_label("Connect");
        let ssid_clone = ssid.clone();
        let gesture = GestureClick::new();
        gesture.set_button(1);
        gesture.connect_pressed(move |_, _, _, _| {
            let network = NetworkService::global();
            // For unknown networks, try connecting without password first
            // (password prompt handling would be done elsewhere if needed)
            network.connect_to_ssid(&ssid_clone, None);
        });
        action_label.add_controller(gesture);
        return action_label.upcast();
    }

    // Known or active networks: hamburger menu with multiple actions.
    let menu_btn = create_row_menu_button();

    let is_active_clone = is_active;
    let is_known_clone = is_known;
    let ssid_for_actions = ssid.clone();

    menu_btn.connect_clicked(move |btn| {
        let popover = Popover::new();
        configure_popover(&popover);

        let panel = GtkBox::new(Orientation::Vertical, 0);
        panel.add_css_class(surface::WIDGET_MENU_CONTENT);

        let content_box = GtkBox::new(Orientation::Vertical, 2);
        content_box.add_css_class(qs::ROW_MENU_CONTENT);

        // Connect / Disconnect actions
        if is_active_clone {
            let ssid_clone = ssid_for_actions.clone();
            let action = create_row_menu_action("Disconnect", move || {
                let network = NetworkService::global();
                debug!("wifi_disconnect_from_menu ssid={}", ssid_clone);
                network.disconnect();
            });
            content_box.append(&action);
        } else {
            let ssid_clone = ssid_for_actions.clone();
            let is_known_inner = is_known_clone;
            let action = create_row_menu_action("Connect", move || {
                let network = NetworkService::global();
                debug!("wifi_connect_from_menu ssid={}", ssid_clone);
                // For known networks or open networks, connect without password
                // Password prompt handling would be done elsewhere if needed
                let _ = is_known_inner; // Kept for future password prompt logic
                network.connect_to_ssid(&ssid_clone, None);
            });
            content_box.append(&action);
        }

        // Forget action for known networks
        if is_known_clone {
            let ssid_clone = ssid_for_actions.clone();
            let action = create_row_menu_action("Forget", move || {
                let network = NetworkService::global();
                debug!("wifi_forget_from_menu ssid={}", ssid_clone);
                network.forget_network(&ssid_clone);
            });
            content_box.append(&action);
        }

        panel.append(&content_box);
        SurfaceStyleManager::global().apply_surface_styles(&panel, true);

        popover.set_child(Some(&panel));
        popover.set_parent(btn);
        popover.popup();

        // Unparent popover when closed to avoid "still has children" warning
        // when the button is destroyed during list refresh
        popover.connect_closed(|p| {
            p.unparent();
        });
    });

    menu_btn.upcast()
}

/// Show inline Wi-Fi password dialog for the given SSID.
pub fn show_password_dialog(state: &WifiCardState, ssid: &str) {
    let ssid = ssid.trim();
    if ssid.is_empty() {
        return;
    }

    *state.password_target_ssid.borrow_mut() = Some(ssid.to_string());

    if let Some(label) = state.password_label.borrow().as_ref() {
        label.set_label(&format!("Enter password for {}", ssid));
    }

    if let Some(entry) = state.password_entry.borrow().as_ref() {
        entry.set_text("");
    }

    if let Some(list_box) = state.base.list_box.borrow().as_ref() {
        let snapshot = NetworkService::global().snapshot();
        populate_wifi_list(state, list_box, &snapshot);
    }
}

/// Called when the password entry is mapped; grabs focus if we have a target.
fn on_password_entry_mapped(state: &WifiCardState, entry: &Entry) {
    if state.password_target_ssid.borrow().is_some() {
        entry.grab_focus();
    }
}

/// Cancel the inline password prompt.
fn on_password_cancel_clicked(state: &WifiCardState, _window: WeakRef<ApplicationWindow>) {
    if let Some(entry) = state.password_entry.borrow().as_ref() {
        entry.set_text("");
    }
    if let Some(box_) = state.password_box.borrow().as_ref() {
        box_.set_visible(false);
    }
    *state.password_target_ssid.borrow_mut() = None;

    if let Some(list_box) = state.base.list_box.borrow().as_ref() {
        let snapshot = NetworkService::global().snapshot();
        populate_wifi_list(state, list_box, &snapshot);
    }
}

/// Attempt to connect using the inline password prompt.
fn on_password_connect_clicked(state: &WifiCardState, _window: WeakRef<ApplicationWindow>) {
    let ssid_opt = state.password_target_ssid.borrow().clone();
    let Some(ssid) = ssid_opt else {
        return;
    };

    let password = if let Some(entry) = state.password_entry.borrow().as_ref() {
        entry.text().to_string()
    } else {
        String::new()
    };

    if ssid.is_empty() {
        return;
    }

    let service = NetworkService::global();
    service.connect_to_ssid(&ssid, Some(&password));

    if let Some(box_) = state.password_box.borrow().as_ref() {
        box_.set_visible(false);
    }
    if let Some(entry) = state.password_entry.borrow().as_ref() {
        entry.set_text("");
    }
    *state.password_target_ssid.borrow_mut() = None;

    if let Some(list_box) = state.base.list_box.borrow().as_ref() {
        let snapshot = NetworkService::global().snapshot();
        populate_wifi_list(state, list_box, &snapshot);
    }
}

/// Update the scan button UI and animate while scanning.
pub fn update_scan_ui(
    state: &WifiCardState,
    snapshot: &NetworkSnapshot,
    window: &ApplicationWindow,
) {
    let scanning = snapshot.scanning;

    // Update label text and CSS
    if let Some(label) = state.scan_label.borrow().as_ref() {
        if scanning {
            label.add_css_class(state::SCANNING);
        } else {
            label.set_label("Scan");
            label.remove_css_class(state::SCANNING);
        }
    }

    // Update button sensitivity
    if let Some(button) = state.scan_button.borrow().as_ref() {
        button.set_sensitive(!scanning);
    }

    // Manage animation timeout
    let mut source_opt = state.scan_anim_source.borrow_mut();
    if scanning {
        if source_opt.is_none() {
            // Start a simple dot animation: "Scanning", "Scanning.", ...
            let step_cell = state.scan_anim_step.clone();
            let source_id = glib::timeout_add_local(std::time::Duration::from_millis(450), {
                let window_weak = window.downgrade();
                move || {
                    if let Some(window) = window_weak.upgrade() {
                        // SAFETY: We store a Weak<QuickSettingsWindow> on the window at creation
                        // time with key "vibepanel-qs-window". upgrade() returns None if dropped.
                        unsafe {
                            if let Some(weak_ptr) = window
                                .data::<Weak<super::window::QuickSettingsWindow>>(
                                    "vibepanel-qs-window",
                                )
                                && let Some(qs) = weak_ptr.as_ref().upgrade()
                                && let Some(label) = qs.wifi.scan_label.borrow().as_ref()
                            {
                                let step = step_cell.get().wrapping_add(1) % 4;
                                step_cell.set(step);
                                let dots = match step {
                                    1 => ".",
                                    2 => "..",
                                    3 => "...",
                                    _ => "",
                                };
                                label.set_label(&format!("Scanning{}", dots));
                            }
                        }
                    }
                    glib::ControlFlow::Continue
                }
            });
            *source_opt = Some(source_id);
        }
    } else if let Some(id) = source_opt.take() {
        id.remove();
        state.scan_anim_step.set(0);
    }
}

/// Handle network state changes from NetworkService.
pub fn on_network_changed(
    state: &WifiCardState,
    snapshot: &NetworkSnapshot,
    window: &ApplicationWindow,
) {
    // Update Wi-Fi toggle state (with signal blocking to prevent feedback loop)
    if let Some(toggle) = state.base.toggle.borrow().as_ref() {
        let enabled = snapshot.wifi_enabled.unwrap_or(false);
        if toggle.is_active() != enabled {
            // Set the flag to block the toggle signal handler
            state.updating_toggle.set(true);
            toggle.set_active(enabled);
            state.updating_toggle.set(false);
        }
    }

    // Update Wi-Fi card icon and its active state class
    if let Some(icon_handle) = state.base.card_icon.borrow().as_ref() {
        let enabled = snapshot.wifi_enabled.unwrap_or(false);
        let icon_name = wifi_icon_name(snapshot.connected, enabled);
        icon_handle.set_icon(icon_name);

        let icon_active = enabled && snapshot.connected;
        set_icon_active(icon_handle, icon_active);

        // Additional disabled styling for Wi-Fi
        let backend = icon_handle.backend_widget();
        if !enabled {
            backend.add_css_class(qs::WIFI_DISABLED_ICON);
        } else {
            backend.remove_css_class(qs::WIFI_DISABLED_ICON);
        }
    }

    // Update Wi-Fi subtitle
    if let Some(label) = state.base.subtitle.borrow().as_ref() {
        let connected = snapshot.ssid.is_some();
        let enabled = snapshot.wifi_enabled.unwrap_or(false);
        let subtitle = if let Some(ref ssid) = snapshot.ssid {
            ssid.clone()
        } else if enabled {
            "Enabled".to_string()
        } else {
            "Disabled".to_string()
        };
        label.set_label(&subtitle);
        set_subtitle_active(label, enabled && connected);
    }

    // Update scan button UI (label + animation)
    update_scan_ui(state, snapshot, window);

    // Update network list
    if let Some(list_box) = state.base.list_box.borrow().as_ref() {
        populate_wifi_list(state, list_box, snapshot);
        // Apply Pango font attrs to dynamically created list rows
        SurfaceStyleManager::global().apply_pango_attrs_all(list_box);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wifi_icon_name_connected() {
        assert_eq!(
            wifi_icon_name(true, true),
            "network-wireless-signal-excellent-symbolic"
        );
    }

    #[test]
    fn test_wifi_icon_name_disconnected() {
        assert_eq!(
            wifi_icon_name(false, true),
            "network-wireless-offline-symbolic"
        );
    }

    #[test]
    fn test_wifi_icon_name_disabled() {
        assert_eq!(
            wifi_icon_name(true, false),
            "network-wireless-offline-symbolic"
        );
        assert_eq!(
            wifi_icon_name(false, false),
            "network-wireless-offline-symbolic"
        );
    }

    #[test]
    fn test_wifi_strength_icon_excellent() {
        assert_eq!(
            wifi_strength_icon(100),
            "network-wireless-signal-excellent-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(80),
            "network-wireless-signal-excellent-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(70),
            "network-wireless-signal-excellent-symbolic"
        );
    }

    #[test]
    fn test_wifi_strength_icon_good() {
        assert_eq!(
            wifi_strength_icon(69),
            "network-wireless-signal-good-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(60),
            "network-wireless-signal-good-symbolic"
        );
    }

    #[test]
    fn test_wifi_strength_icon_ok() {
        assert_eq!(
            wifi_strength_icon(59),
            "network-wireless-signal-ok-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(40),
            "network-wireless-signal-ok-symbolic"
        );
    }

    #[test]
    fn test_wifi_strength_icon_weak() {
        assert_eq!(
            wifi_strength_icon(39),
            "network-wireless-signal-weak-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(20),
            "network-wireless-signal-weak-symbolic"
        );
    }

    #[test]
    fn test_wifi_strength_icon_none() {
        assert_eq!(
            wifi_strength_icon(19),
            "network-wireless-signal-none-symbolic"
        );
        assert_eq!(
            wifi_strength_icon(0),
            "network-wireless-signal-none-symbolic"
        );
    }
}
