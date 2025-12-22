# CSS Variables Reference

vibepanel uses CSS custom properties (variables) for theming. All variables are generated from your configuration and set on the `:root` element.

## Configuration Sources

Variables are computed from these config sections:

- `theme.mode` - "dark", "light", "gtk", or "auto"
- `theme.accent` - "gtk", "none", or a hex color like "#ff6b6b"
- `theme.bar_background_color` - Optional override for bar background
- `theme.widget_background_color` - Optional override for widget backgrounds
- `theme.bar_opacity` / `theme.widget_opacity` - Transparency (0.0-1.0)
- `theme.states.success/warning/urgent` - Semantic state colors
- `theme.typography.font_family` - Font stack
- `bar.size` - Bar height in pixels (sizes scale from this)
- `bar.border_radius` / `widgets.border_radius` - Corner roundness (%)

---

## Background Colors

| Variable | Description |
|----------|-------------|
| `--color-background-bar` | Bar background (with opacity applied via `color-mix`) |
| `--color-background-widget` | Widget/popover background |

**GTK mode**: References `@window_bg_color` and `@view_bg_color` from your GTK theme.

---

## Foreground Colors

| Variable | Description | Typical Use |
|----------|-------------|-------------|
| `--color-foreground-primary` | Full-opacity text | Primary labels, icons |
| `--color-foreground-muted` | 70% opacity | Secondary text, subtitles |
| `--color-foreground-subtle` | 40% opacity | Tertiary/hint text |
| `--color-foreground-disabled` | 40% opacity | Disabled controls |

Use via semantic classes: `.vp-primary`, `.vp-muted`

---

## Accent Colors

| Variable | Description |
|----------|-------------|
| `--color-accent-primary` | Primary accent (buttons, highlights) |
| `--color-accent-subtle` | 20% accent (hover backgrounds) |
| `--color-accent-slider` | Slider fill color (alias for override) |
| `--color-accent-text` | Text on accent backgrounds |

**Accent modes**:
- `gtk` - Uses `@accent_color` from GTK theme
- `none` - Monochrome mode (white/black based on theme)
- `#hexcolor` - Custom accent color

Use via `.vp-accent` class.

---

## State Colors

| Variable | Description | Default |
|----------|-------------|---------|
| `--color-state-success` | Success/positive states | `#4a7a4a` |
| `--color-state-warning` | Warning/caution states | `#e5c07b` |
| `--color-state-urgent` | Error/critical states | `#ff6b6b` |

Use via `.vp-success`, `.vp-warning`, `.vp-urgent` classes.

---

## Card Overlays

Semi-transparent overlays for interactive states on cards/buttons:

| Variable | Description | Typical Opacity |
|----------|-------------|-----------------|
| `--color-card-overlay` | Base card overlay | 6% (dark) / 14% (light) |
| `--color-card-overlay-hover` | Hover state | 2.2x base |
| `--color-card-overlay-subtle` | Subtle highlight | 0.5x base |
| `--color-card-overlay-strong` | Active/pressed | 2x base |
| `--color-click-catcher-overlay` | Click catcher background | ~0.5% |

---

## Row Backgrounds

| Variable | Description |
|----------|-------------|
| `--color-row-background` | Default row background (aliases `--color-card-overlay-subtle`) |
| `--color-row-background-hover` | Row hover (aliases `--color-card-overlay-hover`) |
| `--color-row-critical-background` | Critical/error row (18% urgent blend) |
| `--color-toast-critical-background` | Critical toast background |

---

## Borders & Shadows

| Variable | Description |
|----------|-------------|
| `--color-border-subtle` | Subtle borders (10-12% opacity) |
| `--shadow-soft` | Soft drop shadow (menus, popovers) |
| `--shadow-strong` | Stronger shadow (elevated surfaces) |

---

## Slider Tracks

| Variable | Description |
|----------|-------------|
| `--color-slider-track` | Unfilled track portion |
| `--color-slider-track-disabled` | Track when disabled |

---

## Border Radii

| Variable | Description | Computed From |
|----------|-------------|---------------|
| `--radius-bar` | Bar corner radius | `bar.border_radius` % of rendered height |
| `--radius-surface` | Popover/menu radius | Same as widget |
| `--radius-widget` | Widget corner radius | `widgets.border_radius` % of rendered height |
| `--radius-pill` | Small pill shapes | Half of widget radius |

---

## Sizes & Spacing

All sizes scale proportionally from `bar.size`:

| Variable | Description | Scale Factor |
|----------|-------------|--------------|
| `--bar-height` | Bar height | Direct from config |
| `--bar-padding` | Padding around bar | 14% of bar size |
| `--widget-height` | Widget height | bar - 2×padding |
| `--widget-padding-x` | Horizontal padding | 14% of bar size |
| `--widget-padding-y` | Vertical padding | Fixed 2px |
| `--spacing-internal` | Internal element spacing | 25% of bar size |
| `--spacing-widget-edge` | Widget content edge padding | Fixed 6px |
| `--spacing-widget-gap` | Gap between widget children | Derived from spacing |
| `--widget-opacity` | Widget background opacity | From config |

---

## Typography

| Variable | Description |
|----------|-------------|
| `--font-family` | Font stack from config |
| `--font-scale` | Base scale factor (0.6) |
| `--font-size` | Computed from widget height |
| `--font-size-text-icon` | Text-based icon size |

### Font Size Scale

Relative sizes for visual hierarchy:

| Variable | Value | Use Case |
|----------|-------|----------|
| `--font-size-lg` | 1.1em | Headings, section titles |
| `--font-size-base` | 1.0em | Primary content |
| `--font-size-md` | 0.9em | Row titles |
| `--font-size-sm` | 0.85em | Secondary text |
| `--font-size-xs` | 0.7em | Timestamps, week numbers |

---

## Icon Sizes

| Variable | Description | Scale Factor |
|----------|-------------|--------------|
| `--icon-size` | Standard icon box size | 50% of bar size |
| `--pixmap-icon-size` | System tray icons | 50% of bar size |

---

## Example: Custom Theme Override

In your user stylesheet (`~/.config/vibepanel/style.css`):

```css
/* Custom accent color */
:root {
    --color-accent-primary: #e06c75;
    --color-accent-slider: #98c379;
}

/* Custom state colors */
:root {
    --color-state-success: #98c379;
    --color-state-warning: #d19a66;
    --color-state-urgent: #e06c75;
}

/* Adjust slider accent separately */
.brightness-slider trough highlight {
    background-image: image(var(--color-accent-slider));
}
```

---

## CSS Classes

### Widget Structure

| Class | Element | Description |
|-------|---------|-------------|
| `.widget` | Widget container | Individual widget island with background, padding, border-radius |
| `.content` | Inner box | Direct child of `.widget`, contains icons/labels |
| `.widget-group` | Group container | Applied alongside `.widget` when multiple widgets share an island |

### Widget Grouping

When you group widgets in config:

```toml
right = [
  { group = [{ name = "battery" }, { name = "clock" }] },
]
```

The resulting DOM structure is:

```
.widget.widget-group
  └── .content (group's content box)
        ├── [battery widget box]
        │     └── .content (battery's content)
        │           ├── icon
        │           └── label
        └── [clock widget box]
              └── .content (clock's content)
                    └── label
```

**Customizing grouped widgets:**

```css
/* Different padding for grouped islands */
.widget.widget-group {
    padding: 0 6px;
}

/* Different border radius for groups */
.widget.widget-group {
    border-radius: 999px;
}

/* Add visual separator between grouped widgets */
.widget-group > .content > *:not(:last-child) {
    border-right: 1px solid var(--color-border-subtle);
    padding-right: 8px;
}
```

---

## Internal Constants

These are not exposed as CSS variables but control the computed values:

| Constant | Value | Description |
|----------|-------|-------------|
| `FONT_SCALE` | 0.6 | Font size as % of widget height |
| `TEXT_ICON_SCALE` | 0.50 | Text icon size as % of bar |
| `PIXMAP_ICON_SCALE` | 0.50 | Pixmap icon size as % of bar |
| `PADDING_SCALE` | 0.14 | Padding as % of bar |
| `SPACING_SCALE` | 0.25 | Spacing as % of bar |
| `FOREGROUND_MUTED_OPACITY` | 0.7 | Muted text opacity |
| `FOREGROUND_SUBTLE_OPACITY` | 0.4 | Subtle text opacity |
