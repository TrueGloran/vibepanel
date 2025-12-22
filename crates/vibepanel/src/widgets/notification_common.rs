//! Common utilities shared between notification widget modules.
//!
//! This module contains constants and helper functions used by both
//! notification_toast.rs and notification_popover.rs.

use gtk4::Image;

use crate::services::icons::get_app_icon_name;
use crate::services::notification::{Notification, NotificationImage};
use std::time::{SystemTime, UNIX_EPOCH};

/// Toast display duration in ms
pub const TOAST_TIMEOUT_MS: u32 = 5000;
/// Critical notifications don't auto-dismiss
pub const TOAST_TIMEOUT_CRITICAL_MS: u32 = 0;

/// Estimated height per toast (including padding/margins) for stack positioning
pub const TOAST_ESTIMATED_HEIGHT: i32 = 85;
pub const TOAST_GAP: i32 = 4;
pub const TOAST_MARGIN_TOP: i32 = 10;
pub const TOAST_MARGIN_RIGHT: i32 = 10;

/// Popover dimensions
pub const POPOVER_WIDTH: i32 = 400;
pub const POPOVER_ROW_HEIGHT: i32 = 100;
pub const POPOVER_MAX_VISIBLE_ROWS: i32 = 3;

/// Threshold for body text length before we show the expand button.
/// Bodies shorter than this are shown in full without expand/collapse UI.
pub const BODY_TRUNCATE_THRESHOLD: usize = 80;

/// Format a timestamp as a human-readable relative time.
pub fn format_timestamp(timestamp: f64) -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0);
    let diff = now - timestamp;

    if diff < 60.0 {
        "Just now".to_string()
    } else if diff < 3600.0 {
        let mins = (diff / 60.0) as i32;
        format!("{}m ago", mins)
    } else if diff < 86400.0 {
        let hours = (diff / 3600.0) as i32;
        format!("{}h ago", hours)
    } else {
        let days = (diff / 86400.0) as i32;
        format!("{}d ago", days)
    }
}

/// Truncate notification body text for display.
pub fn truncate_body(body: &str, max_chars: usize) -> String {
    let text = body.replace('\n', " ");
    let text = text.trim();
    if text.len() > max_chars {
        format!("{}...", &text[..max_chars])
    } else {
        text.to_string()
    }
}

/// Create an Image widget for a notification, preferring avatar data
/// from image-data/image-path hints when available.
pub fn create_notification_image_widget(notification: &Notification) -> Image {
    // Fixed size for notification avatars/icons (larger than theme default)
    const NOTIFICATION_ICON_SIZE: i32 = 48;

    // Try raw image-data first (e.g. chat avatar from Telegram)
    if let Some(ref img) = notification.image_data
        && let Some(texture) = notification_image_to_texture(img)
    {
        let image = Image::from_paintable(Some(&texture));
        image.set_pixel_size(NOTIFICATION_ICON_SIZE);
        return image;
    }

    // Note: image-path can be either an actual file path OR an icon theme name
    if let Some(ref path) = notification.image_path {
        let image = if let Some(file_path) = path.strip_prefix("file://") {
            // file:// URI - load from filesystem
            Image::from_file(file_path)
        } else if path.starts_with('/') {
            // Absolute path - load from filesystem
            Image::from_file(path)
        } else {
            // Icon theme name - use icon theme lookup
            Image::from_icon_name(path)
        };

        image.set_pixel_size(NOTIFICATION_ICON_SIZE);
        return image;
    }

    // Finally, fall back to icon theme / desktop entry logic
    create_notification_icon(
        &notification.app_icon,
        &notification.app_name,
        notification.desktop_entry.as_deref(),
    )
}

/// Convert raw NotificationImage data into a gdk Texture.
fn notification_image_to_texture(img: &NotificationImage) -> Option<gtk4::gdk::Texture> {
    use gtk4::gdk;
    use gtk4::glib::Bytes;
    use gtk4::prelude::*;

    if img.width <= 0 || img.height <= 0 || img.data.is_empty() {
        return None;
    }

    // The freedesktop notification spec uses RGBA format (not ARGB like StatusNotifierItem).
    // Pass the raw bytes directly without conversion.
    let bytes = Bytes::from(&img.data[..]);

    let format = if img.has_alpha && img.channels == 4 {
        gdk::MemoryFormat::R8g8b8a8
    } else {
        // 3-channel RGB (rare, but handle it)
        gdk::MemoryFormat::R8g8b8
    };

    let texture = gdk::MemoryTexture::new(
        img.width,
        img.height,
        format,
        &bytes,
        img.rowstride as usize,
    );

    Some(texture.upcast())
}

/// Create an icon widget for a notification.
///
/// Resolution precedence:
///   1. app_icon (if non-empty)
///   2. desktop_entry hint (e.g. "org.telegram.desktop")
///   3. app_name via desktop entry lookup
///   4. generic fallback icon
fn create_notification_icon(app_icon: &str, app_name: &str, desktop_entry: Option<&str>) -> Image {
    // Fixed size for notification icons (larger than theme default)
    const NOTIFICATION_ICON_SIZE: i32 = 48;

    let fallback = "dialog-information-symbolic";

    // Determine which icon to use:
    // 1. If app_icon is provided (non-empty), use it
    // 2. Otherwise, try to resolve from desktop_entry via icons service
    // 3. Otherwise, try to resolve from app_name via desktop entry lookup
    // 4. Fall back to generic icon
    let icon_name = if !app_icon.is_empty() {
        app_icon.to_string()
    } else if let Some(desktop) = desktop_entry {
        let resolved = get_app_icon_name(desktop);
        if resolved.is_empty() {
            fallback.to_string()
        } else {
            resolved
        }
    } else if !app_name.is_empty() {
        let resolved = get_app_icon_name(app_name);
        if resolved.is_empty() {
            fallback.to_string()
        } else {
            resolved
        }
    } else {
        fallback.to_string()
    };

    // Handle file:// URIs
    if let Some(file_path) = icon_name.strip_prefix("file://") {
        let icon = Image::from_file(file_path);
        icon.set_pixel_size(NOTIFICATION_ICON_SIZE);
        return icon;
    }

    // Handle absolute file paths
    if icon_name.starts_with('/') {
        let icon = Image::from_file(&icon_name);
        icon.set_pixel_size(NOTIFICATION_ICON_SIZE);
        return icon;
    }

    // It's an icon theme name
    let icon = Image::from_icon_name(&icon_name);
    icon.set_pixel_size(NOTIFICATION_ICON_SIZE);
    icon
}
