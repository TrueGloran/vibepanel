//! Quick Settings UI helpers - shared card/row builders.
//!
//! Provides reusable UI builders for the quick settings control center panels.

use std::cell::RefCell;
use std::rc::Rc;

use crate::services::icons::{IconHandle, IconsService};
use crate::styles::{button, color, qs, row, state};
use gtk4::prelude::*;
use gtk4::{
    Align, Box as GtkBox, Button, Label, ListBox, ListBoxRow, Orientation, Revealer, SelectionMode,
    ToggleButton,
};

/// Base state for expandable cards (Wi-Fi, Bluetooth, VPN).
///
/// This struct contains the common fields shared by all expandable cards
/// in the Quick Settings panel. Card-specific state should be stored
/// separately and composed with this base.
#[derive(Default)]
pub struct ExpandableCardBase {
    /// The toggle button for power on/off.
    pub toggle: RefCell<Option<ToggleButton>>,
    /// The card icon handle for dynamic updates.
    pub card_icon: RefCell<Option<IconHandle>>,
    /// The subtitle label showing current status.
    pub subtitle: RefCell<Option<Label>>,
    /// The list box containing items (networks/devices/connections).
    pub list_box: RefCell<Option<ListBox>>,
    /// The revealer for accordion expand/collapse.
    pub revealer: RefCell<Option<Revealer>>,
    /// The arrow icon handle for expand indicator.
    pub arrow: RefCell<Option<IconHandle>>,
}

impl ExpandableCardBase {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Trait for expandable card state types.
///
/// This trait provides access to the common base fields and allows
/// the AccordionManager to work with different card types uniformly.
pub trait ExpandableCard {
    /// Get a reference to the base state.
    fn base(&self) -> &ExpandableCardBase;
}

/// Set the active state styling on an icon handle's backend widget.
///
/// When active, applies `qs-icon-active` and removes `vp-primary`.
/// When inactive, removes `qs-icon-active` and adds `vp-primary`.
///
/// This should be called on the `backend_widget()` of an IconHandle, not
/// the container `widget()`, because `vp-primary` is applied directly to
/// the backend (Label/Image) during icon creation.
pub fn set_icon_active(icon_handle: &IconHandle, active: bool) {
    let backend = icon_handle.backend_widget();
    if active {
        backend.add_css_class(state::ICON_ACTIVE);
        backend.remove_css_class(color::PRIMARY);
    } else {
        backend.remove_css_class(state::ICON_ACTIVE);
        backend.add_css_class(color::PRIMARY);
    }
}

/// Set the active state styling on a subtitle label.
///
/// When active, applies `qs-subtitle-active` (accent color).
/// When inactive, removes `qs-subtitle-active`.
pub fn set_subtitle_active(label: &Label, active: bool) {
    if active {
        label.add_css_class(state::SUBTITLE_ACTIVE);
    } else {
        label.remove_css_class(state::SUBTITLE_ACTIVE);
    }
}

/// Manages accordion behavior for expandable cards.
///
/// The accordion ensures only one card is expanded at a time. When a card
/// is expanded, all other registered cards are collapsed instantly.
pub struct AccordionManager {
    /// Registered expandable cards (stored as trait objects).
    cards: RefCell<Vec<Rc<dyn ExpandableCard>>>,
}

impl AccordionManager {
    /// Create a new accordion manager.
    pub fn new() -> Self {
        Self {
            cards: RefCell::new(Vec::new()),
        }
    }

    /// Register an expandable card with the accordion.
    pub fn register<T: ExpandableCard + 'static>(&self, card: Rc<T>) {
        self.cards.borrow_mut().push(card);
    }

    /// Collapse all cards except the one with the given revealer.
    ///
    /// This should be called when a card is about to expand.
    pub fn collapse_others(&self, except_revealer: &Revealer) {
        for card in self.cards.borrow().iter() {
            let base = card.base();
            if let Some(revealer) = base.revealer.borrow().as_ref() {
                // Skip the card that's being expanded
                if revealer == except_revealer {
                    continue;
                }
                // Collapse this card if it's expanded
                if revealer.reveals_child() {
                    collapse_revealer_instant(revealer);
                    if let Some(arrow) = base.arrow.borrow().as_ref() {
                        arrow.widget().remove_css_class(state::EXPANDED);
                    }
                }
            }
        }
    }

    /// Set up accordion behavior for a card's expander button.
    ///
    /// This connects the expander button to toggle the revealer and
    /// automatically collapse other cards when expanding.
    pub fn setup_expander<T: ExpandableCard + 'static>(
        accordion: &Rc<Self>,
        card: &Rc<T>,
        expander_btn: &Button,
    ) {
        let accordion = Rc::clone(accordion);
        let revealer = card.base().revealer.borrow().clone();
        let arrow = card.base().arrow.borrow().clone();

        expander_btn.connect_clicked(move |_| {
            let Some(revealer) = revealer.as_ref() else {
                return;
            };

            let expanding = !revealer.reveals_child();

            // Collapse other cards first (accordion behavior)
            if expanding {
                accordion.collapse_others(revealer);
            }

            revealer.set_reveal_child(expanding);

            if let Some(ref arrow) = arrow {
                if expanding {
                    arrow.widget().add_css_class(state::EXPANDED);
                } else {
                    arrow.widget().remove_css_class(state::EXPANDED);
                }
            }
        });
    }
}

impl Default for AccordionManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Add a placeholder row to a list box (e.g., "No networks found").
pub fn add_placeholder_row(list_box: &ListBox, text: &str) {
    let label = Label::new(Some(text));
    label.add_css_class(qs::MUTED_LABEL);
    label.add_css_class(color::MUTED);
    label.set_xalign(0.0);

    let list_row = ListBoxRow::new();
    list_row.set_child(Some(&label));
    list_row.set_activatable(false);
    list_box.append(&list_row);
}

/// Create a hamburger menu button for list rows with multiple actions.
///
/// # CSS Classes Applied
///
/// - `.qs-row-menu-button` and `.vp-btn-reset` on the button
/// - `.qs-row-menu-icon` on the menu icon
pub fn create_row_menu_button() -> Button {
    let menu_btn = Button::new();
    menu_btn.set_has_frame(false);
    menu_btn.add_css_class(row::QS_MENU_BUTTON);
    menu_btn.add_css_class(button::RESET);

    // Use IconsService so Material mapping is applied
    let icons = IconsService::global();
    let icon_handle = icons.create_icon("open-menu-symbolic", &[row::QS_MENU_ICON, color::PRIMARY]);

    // Center the icon within the button's hover area
    let icon_widget = icon_handle.widget();
    icon_widget.set_halign(gtk4::Align::Center);
    icon_widget.set_valign(gtk4::Align::Center);
    menu_btn.set_child(Some(&icon_widget));

    menu_btn
}

/// Create a single inline action as accent-colored text (no background).
///
/// Use this for rows with only one action (e.g., VPN Connect/Disconnect,
/// Wi-Fi Connect for unknown networks).
///
/// # CSS Classes Applied
///
/// - `.qs-row-action-label` on the button
pub fn create_row_action_label(label_text: &str) -> Button {
    let btn = Button::with_label(label_text);
    btn.set_has_frame(false);
    btn.add_css_class(row::QS_ACTION_LABEL);
    btn.add_css_class(color::ACCENT);
    btn
}

/// Create a menu action button for row context menus.
///
/// Use this inside popover menus created from `create_row_menu_button`.
/// The button has a left-aligned label and ghost styling.
///
/// # CSS Classes Applied
///
/// - `.qs-row-menu-item` and `.vp-btn-ghost` on the button
/// - `.vp-primary` on the label
pub fn create_row_menu_action<F>(label_text: &str, on_click: F) -> Button
where
    F: Fn() + 'static,
{
    let btn = Button::new();
    btn.set_has_frame(false);
    btn.set_focusable(false);
    btn.set_focus_on_click(false);
    btn.add_css_class(qs::ROW_MENU_ITEM);
    btn.add_css_class(button::GHOST);

    let lbl = Label::new(Some(label_text));
    lbl.set_xalign(0.0);
    lbl.add_css_class(color::PRIMARY);
    btn.set_child(Some(&lbl));

    btn.connect_clicked(move |_| {
        on_click();
    });

    btn
}

/// Collapse a revealer instantly without animation.
///
/// This temporarily sets the transition duration to 0, collapses the revealer,
/// then restores the original duration. Used for accordion behavior where
/// closing other panels should be instant while the active panel animates open.
pub fn collapse_revealer_instant(revealer: &Revealer) {
    if revealer.reveals_child() {
        let old_dur = revealer.transition_duration();
        revealer.set_transition_duration(0);
        revealer.set_reveal_child(false);
        revealer.set_transition_duration(old_dur);
    }
}

/// Clear all children from a ListBox.
pub fn clear_list_box(list_box: &ListBox) {
    while let Some(child) = list_box.first_child() {
        list_box.remove(&child);
    }
}

/// Create a new ListBox configured for quick settings panels.
///
/// # CSS Classes Applied
///
/// - `.qs-list` on the list box
pub fn create_qs_list_box() -> ListBox {
    let list_box = ListBox::new();
    list_box.add_css_class(qs::LIST);
    list_box.set_selection_mode(SelectionMode::None);
    list_box
}

/// Result of building a scan/refresh button.
pub struct ScanButtonResult {
    /// The button widget.
    pub button: Button,
    /// The label inside the button (for animation updates).
    pub label: Label,
}

/// Build a scan/refresh button styled consistently across cards.
///
/// This creates the button used in Wi-Fi ("Scan"), Bluetooth ("Scan"),
/// and Updates ("Refresh") cards. The label can be updated dynamically
/// for scanning animations.
///
/// # CSS Classes Applied
///
/// - `.qs-scan-button` on the button
/// - `.qs-scan-label` and `.vp-primary` on the label
pub fn build_scan_button(label_text: &str) -> ScanButtonResult {
    let button = Button::new();
    button.add_css_class(qs::SCAN_BUTTON);
    button.set_has_frame(false);
    button.set_halign(Align::Start);

    let content = GtkBox::new(Orientation::Horizontal, 4);
    let label = Label::new(Some(label_text));
    label.add_css_class(qs::SCAN_LABEL);
    label.add_css_class(color::PRIMARY);
    content.append(&label);
    button.set_child(Some(&content));

    ScanButtonResult { button, label }
}
