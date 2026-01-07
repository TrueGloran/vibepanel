# Configuration Reference

vibepanel is configured via a TOML file. This document describes all available options.

## Config File Location

Config files are searched in order:

1. `$XDG_CONFIG_HOME/vibepanel/config.toml`
2. `~/.config/vibepanel/config.toml`
3. `./config.toml` (current directory)

Use `--config <path>` to specify an explicit path.

## CLI Commands

```bash
vibepanel --print-default-config  # Print complete reference config with comments
vibepanel --print-config          # Print effective config (user merged with defaults)
vibepanel --check-config          # Validate config and exit
```

---

## Bar Settings

```toml
[bar]
size = 32                # Bar height in pixels (all sizes scale from this)
widget_spacing = 8       # Space between widgets (pixels)
outer_margin = 4         # Distance from screen edge (pixels)
section_edge_margin = 8  # Distance from bar edge to first/last section (pixels)
border_radius = 30       # Corner roundness (% of bar height, 50 = fully rounded)
popover_offset = 1       # Gap between widgets and popovers (pixels)

# Notch mode for displays with a camera notch
notch_enabled = false
notch_width = 195        # Notch width in pixels (0 = auto-detect)

# Limit bar to specific outputs (empty = all outputs)
outputs = []
```

---

## Widget Placement

```toml
[widgets]
left = ["workspace", "window_title"]
center = []
right = ["system_tray", "clock", "notifications"]

# Border radius for widgets (% of widget height)
border_radius = 40
```

### Available Widgets

| Widget | Description |
|--------|-------------|
| `workspace` | Workspace indicators |
| `window_title` | Active window title |
| `clock` | Date and time |
| `battery` | Battery status |
| `cpu` | CPU usage |
| `memory` | Memory usage |
| `system_tray` | System tray icons |
| `notifications` | Notification center |
| `quick_settings` | Quick settings panel |
| `updates` | System updates indicator |

### Widget Groups

Widgets can be grouped to share a single visual "island":

```toml
right = [
  "system_tray",
  { group = ["cpu", "memory"] },
  { group = ["battery", "clock"] },
  "notifications",
]
```

### Notch Mode

When `notch_enabled = true`, use `center_left` and `center_right` instead of `center`:

```toml
[bar]
notch_enabled = true
notch_width = 195

[widgets]
center_left = ["window_title"]
center_right = ["clock"]
```

---

## Per-Widget Options

Configure individual widgets with `[widgets.<name>]` sections. All widgets support `disabled = true` to hide them.

### clock

```toml
[widgets.clock]
format = "%a %d %H:%M"   # strftime format (default: "Mon 21 14:30")
show_week_numbers = false   # hide week numbers
```

See [strftime.org](https://strftime.org) for format codes.

### battery

```toml
[widgets.battery]
show_percentage = true   # Show "85%" text
show_icon = true         # Show battery icon
```

### workspace

```toml
[widgets.workspace]
label_type = "none"      # "icons", "numbers", or "none"
separator = ""           # Separator between indicators
```

Label types:

- `icons` - Symbols: `●` (active), `○` (occupied), `◆` (urgent)
- `numbers` - Workspace names/numbers
- `none` - Minimal (CSS-only styling)

### window_title

```toml
[widgets.window_title]
empty_text = "—"          # Text when no window focused
template = "{display}"    # Title template (see below)
show_app_fallback = true  # Show app name when title is empty
max_chars = 0             # Max characters (0 = unlimited)
show_icon = true          # Show app icon
uppercase = false         # Uppercase the title
```

Template variables:

- `{title}` - Raw window title
- `{app_id}` - App ID from compositor
- `{app}` - Friendly app name
- `{content}` - Title with app name removed
- `{display}` - Smart combination (default)

### system_tray

```toml
[widgets.system_tray]
max_icons = 12           # Maximum tray icons to show
pixmap_icon_size = 18    # Icon size in pixels
```

### notifications

No additional options. Configure via your notification daemon.

### quick_settings

Toggle individual cards in the quick settings panel:

```toml
[widgets.quick_settings]
wifi = true
bluetooth = true
vpn = true
audio = true
mic = true
brightness = true
power = true
idle_inhibitor = true
updates = true
```

### updates

```toml
[widgets.updates]
check_interval = 3600    # Check interval in seconds (default: 1 hour)
terminal = ""            # Terminal for upgrade command (auto-detected)
```

Supported terminals: `ghostty`, `foot`, `alacritty`, `kitty`, `wezterm`, `gnome-terminal`, `konsole`

### cpu

```toml
[widgets.cpu]
show_icon = true
show_percentage = true
```

### memory

```toml
[widgets.memory]
show_icon = true
format = "percentage"    # "percentage", "absolute", or "both"
```

Format examples:

- `percentage` - "76%"
- `absolute` - "8.2G"
- `both` - "8.2/16G"

---

## Icons

```toml
[icons]
theme = "material"       # "material" (bundled) or "gtk" (system theme)
```

---

## Compositor Backend

```toml
[workspace]
backend = "auto"         # "auto", "mango", "hyprland", or "niri"
```

Auto-detection:

1. `HYPRLAND_INSTANCE_SIGNATURE` env var → `hyprland`
2. `NIRI_SOCKET` env var → `niri`
3. Otherwise → `mango` (MangoWC/DWL)

---

## Theme

```toml
[theme]
mode = "dark"            # "auto", "dark", "light", or "gtk"
accent = "#adabe0"       # "gtk", "none", or hex color

# Background colors (CSS format, optional)
bar_background_color = "#222222"
widget_background_color = "#111217"

# Opacity (0.0 = transparent, 1.0 = opaque)
bar_opacity = 0.0        # Transparent bar for "islands" look
widget_opacity = 1.0

[theme.states]
success = "#4a7a4a"      # e.g., active workspace
warning = "#e5c07b"      # e.g., medium battery
urgent = "#ff6b6b"       # e.g., low battery, urgent workspace

[theme.typography]
font_family = "monospace"
```

### Theme Modes

| Mode | Description |
|------|-------------|
| `auto` | Detect from widget background luminance |
| `dark` | Light text on dark backgrounds |
| `light` | Dark text on light backgrounds |
| `gtk` | Derive colors from GTK theme |

### Accent Modes

| Value | Description |
|-------|-------------|
| `gtk` | Use GTK theme's accent color |
| `none` | Monochrome (no colored accents) |
| `#rrggbb` | Custom hex color |

---

## On-Screen Display

```toml
[osd]
enabled = true
position = "bottom"      # "bottom", "top", "left", or "right"
timeout_ms = 1500        # Duration in milliseconds
```

---

## Advanced

```toml
[advanced]
pango_font_rendering = false  # Use Pango instead of CSS for fonts
```

Enable `pango_font_rendering` if you see clipped glyphs or sizing issues in layer-shell surfaces.

---

## Custom Styling

Place a `style.css` file in the same directory as `config.toml`. Changes are live-reloaded.

See [css-variables.md](css-variables.md) for available CSS variables and classes.

### Example

```css
/* Custom accent color */
:root {
    --color-accent-primary: #e06c75;
}

/* Style active workspace */
.workspace-indicator.active {
    background-color: var(--color-accent-primary);
}

/* Style urgent workspaces */
.workspace-indicator.urgent {
    background-color: var(--color-state-urgent);
}
```

### Key CSS Classes

| Class | Element |
|-------|---------|
| `.widget` | Widget containers |
| `.widget-group` | Grouped widgets |
| `.workspace-indicator` | Workspace buttons |
| `.workspace-indicator.active` | Active workspace |
| `.workspace-indicator.occupied` | Has windows |
| `.workspace-indicator.urgent` | Urgent notification |
| `.battery-icon` | Battery icon |
| `.notification-icon` | Notification bell |
| `.vp-card` | Quick settings cards |
| `.qs-cc-slider` | Volume/brightness sliders |
