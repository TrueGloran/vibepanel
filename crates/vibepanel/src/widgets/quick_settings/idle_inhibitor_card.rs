//! Idle Inhibitor card for Quick Settings panel.
//!
//! This module contains:
//! - Idle inhibitor state handling (simple toggle card, no expander)

use std::cell::RefCell;

use gtk4::prelude::*;
use gtk4::{Label, ToggleButton};

use crate::services::icons::IconHandle;
use crate::services::idle_inhibitor::IdleInhibitorSnapshot;

use super::ui_helpers::{set_icon_active, set_subtitle_active};

/// State for the Idle Inhibitor card in the Quick Settings panel.
pub struct IdleInhibitorCardState {
    /// Idle inhibitor toggle button.
    pub toggle: RefCell<Option<ToggleButton>>,
    /// Idle inhibitor card icon handle.
    pub card_icon: RefCell<Option<IconHandle>>,
    /// Idle inhibitor subtitle label.
    pub subtitle: RefCell<Option<Label>>,
}

impl IdleInhibitorCardState {
    pub fn new() -> Self {
        Self {
            toggle: RefCell::new(None),
            card_icon: RefCell::new(None),
            subtitle: RefCell::new(None),
        }
    }
}

impl Default for IdleInhibitorCardState {
    fn default() -> Self {
        Self::new()
    }
}

/// Handle Idle Inhibitor state changes from IdleInhibitorService.
pub fn on_idle_inhibitor_changed(state: &IdleInhibitorCardState, snapshot: &IdleInhibitorSnapshot) {
    // Update toggle state
    if let Some(toggle) = state.toggle.borrow().as_ref() {
        if toggle.is_active() != snapshot.active {
            toggle.set_active(snapshot.active);
        }
        toggle.set_sensitive(snapshot.available);
    }

    // Update icon active state
    if let Some(icon_handle) = state.card_icon.borrow().as_ref() {
        set_icon_active(icon_handle, snapshot.active);
    }

    // Update subtitle
    if let Some(label) = state.subtitle.borrow().as_ref() {
        let subtitle = if snapshot.active {
            "Enabled"
        } else {
            "Disabled"
        };
        label.set_label(subtitle);
        set_subtitle_active(label, snapshot.active);
    }
}
