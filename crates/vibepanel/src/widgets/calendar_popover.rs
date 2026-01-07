use std::cell::RefCell;
use std::rc::Rc;

use chrono::{Datelike, Local, NaiveDate};
use gtk4::prelude::*;
use gtk4::{Align, Box as GtkBox, Button, Calendar, Label, Orientation, Overlay, Widget};

use crate::styles::{calendar as cal, surface};

/// Build a calendar popover for the clock widget.
///
/// Shows a month view calendar with custom previous/next navigation and a
/// header label. Toggles a `show-today` CSS class when the currently viewed
/// month matches the real current month.
pub fn build_clock_calendar_popover(show_week_numbers: bool) -> Widget {
    // Today and tracked month/year (always using day = 1 so that
    // month arithmetic is simpler and avoids invalid dates like 31 Feb).
    let today: NaiveDate = Local::now().date_naive();
    let current_date = Rc::new(RefCell::new(today));

    // Main container
    let container = GtkBox::new(Orientation::Vertical, 0);
    container.add_css_class(cal::POPOVER);
    container.add_css_class(surface::NO_FOCUS);

    // Header with navigation

    let header_box = GtkBox::new(Orientation::Horizontal, 8);
    header_box.set_halign(Align::Center);

    // Month/year label - initial text is updated below via helper.
    let header_label = Label::new(None);
    header_label.add_css_class(surface::POPOVER_TITLE);
    header_label.set_valign(Align::Start);

    header_box.append(&header_label);
    container.append(&header_box);

    // Calendar widget
    let calendar = Calendar::new();
    calendar.set_show_heading(false);
    calendar.set_show_week_numbers(show_week_numbers);
    calendar.add_css_class(cal::WIDGET);
    calendar.add_css_class(cal::GRID);
    calendar.set_halign(Align::Fill); // Fill the wrapper so left alignment works relative to it
    // Initially show today styling since we start in the current month
    calendar.add_css_class(cal::SHOW_TODAY);

    // Wrapper to center the calendar+overlay in the popover
    let wrapper = GtkBox::new(Orientation::Vertical, 0);
    wrapper.set_halign(Align::Center);

    if show_week_numbers {
        // Week number header "w"
        // We use an Overlay to position the "w" label precisely over the top-left corner
        // of the calendar, aligning it with the week number column.
        let overlay = Overlay::new();
        overlay.set_child(Some(&calendar));

        let w_label = Label::new(Some("w"));
        w_label.add_css_class("week-number-header");
        w_label.set_halign(Align::Start);
        w_label.set_valign(Align::Start);

        overlay.add_overlay(&w_label);
        wrapper.append(&overlay);
    } else {
        // No week numbers, just append calendar directly
        wrapper.append(&calendar);
    }

    container.append(&wrapper);

    // Helper closures --------------------------------------------------------

    // Update header label text from a NaiveDate (Month YYYY).
    let update_header = {
        let header_label = header_label.clone();
        move |date: NaiveDate| {
            header_label.set_label(&date.format("%B %Y").to_string());
        }
    };

    // Sync the GtkCalendar display and the `show-today` CSS class based on
    // the logical date representing the visible month.
    let update_calendar = {
        let calendar = calendar.clone();
        move |today: NaiveDate, date: NaiveDate| {
            calendar.set_year(date.year());
            // GtkCalendar expects month in the 0-11 range (i32)
            calendar.set_month(date.month0() as i32);

            let is_current_month = date.month() == today.month() && date.year() == today.year();

            if is_current_month {
                calendar.add_css_class(cal::SHOW_TODAY);
            } else {
                calendar.remove_css_class(cal::SHOW_TODAY);
            }
        }
    };

    // Initial sync to today's month.
    {
        let date = *current_date.borrow();
        update_header(date);
        update_calendar(today, date);
    }

    // Navigation buttons (prev/next) ----------------------------------------

    let prev_button = Button::from_icon_name("go-previous-symbolic");
    prev_button.add_css_class(surface::POPOVER_ICON_BTN);
    prev_button.set_valign(Align::Start);
    if let Some(child) = prev_button.child() {
        child.set_halign(gtk4::Align::Center);
        child.set_valign(gtk4::Align::Center);
    }

    {
        let current_date = current_date.clone();
        let update_header = update_header.clone();
        let update_calendar = update_calendar.clone();
        prev_button.connect_clicked(move |_| {
            let mut date = current_date.borrow_mut();
            let mut year = date.year();
            let mut month = date.month();
            if month == 1 {
                month = 12;
                year -= 1;
            } else {
                month -= 1;
            }
            if let Some(new_date) = NaiveDate::from_ymd_opt(year, month, 1) {
                *date = new_date;
                update_header(new_date);
                update_calendar(today, new_date);
            }
        });
    }

    let next_button = Button::from_icon_name("go-next-symbolic");
    next_button.add_css_class(surface::POPOVER_ICON_BTN);
    next_button.set_valign(Align::Start);
    if let Some(child) = next_button.child() {
        child.set_halign(gtk4::Align::Center);
        child.set_valign(gtk4::Align::Center);
    }

    {
        let current_date = current_date.clone();
        let update_header = update_header.clone();
        let update_calendar = update_calendar.clone();
        next_button.connect_clicked(move |_| {
            let mut date = current_date.borrow_mut();
            let mut year = date.year();
            let mut month = date.month();
            if month == 12 {
                month = 1;
                year += 1;
            } else {
                month += 1;
            }
            if let Some(new_date) = NaiveDate::from_ymd_opt(year, month, 1) {
                *date = new_date;
                update_header(new_date);
                update_calendar(today, new_date);
            }
        });
    }

    // Insert buttons around the header label.
    header_box.prepend(&prev_button);
    header_box.append(&next_button);

    // Calendar internal navigation (e.g., selecting a day that moves between
    // months) should also keep `current_date` and the header / CSS in sync.
    {
        let current_date = current_date.clone();
        let update_header = update_header.clone();
        let update_calendar = update_calendar.clone();
        calendar.connect_day_selected(move |cal: &Calendar| {
            let year = cal.year();
            // GtkCalendar months are 0-11
            let month = cal.month() + 1;
            if let Some(date) = NaiveDate::from_ymd_opt(year, month as u32, 1) {
                *current_date.borrow_mut() = date;
                update_header(date);
                update_calendar(today, date);
            }
        });
    }

    container.upcast::<Widget>()
}
