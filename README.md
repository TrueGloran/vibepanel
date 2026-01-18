# VibePanel

<p align="center">
  <img src="assets/screenshots/islands_bar_light.png" alt="VibePanel bar preview" width="830">
</p>

A GTK4 panel for Wayland with integrated notifications, OSD, and quick settings. Supports Hyprland, Niri, MangoWC and DWL.

## Why VibePanel?

VibePanel is something between a simple status bar and a full desktop shell:

- **Minimal config** – Sensible defaults out of the box; customize with TOML, CSS only if needed.
- **Batteries included** – VibePanel replaces several common components with a single binary:
  - **Notifications** – Integrated notification center
  - **OSD** – Built-in on-screen display for volume and brightness
  - **Quick settings** – Native panel for Wi‑Fi, Bluetooth, audio, power profiles and more
- **Modern aesthetics** – Defaults to a floating “island” style with instant hot‑reloading for layouts and themes.
- **Integrated CLI** – Small CLI for controlling volume, brightness and idle inhibition.
- **Center anchoring** – Custom GTK4 layout keeps center widgets centered even when left/right sections grow.

## Screenshots

These examples use roughly ~10–35 lines of TOML to get completely different vibes, no CSS required.

<table align="center">
  <tr>
    <td colspan="2"><img src="assets/screenshots/islands_desktop.png"></td>
  </tr>
  <tr>
    <td colspan="2"><img src="assets/screenshots/gruvbox_desktop.png"></td>
  </tr>
  <tr>
    <td><a href="assets/screenshots/frosted_minimal_desktop.png"><img src="assets/screenshots/frosted_minimal_desktop.png" width="400"></a></td>
    <td><a href="assets/screenshots/sonoma_desktop.png"><img src="assets/screenshots/sonoma_desktop.png" width="400"></a></td>
  </tr>
</table>

## Widgets

- Workspaces - clickable indicators with tooltips
- Window title - active window with app icon
- Clock - configurable format with calendar popover
- Battery - status with detailed popover and power profiles
- Quick settings - audio, brightness, bluetooth, wifi, VPN, power profiles, idle inhibitor
- System tray - XDG tray support
- Notifications - notification center with Do Not Disturb
- Updates - package update indicator (dnf and pacman/paru support right now)
- CPU & Memory - system resource monitors

## Status

VibePanel is in early 0.x development but should be stable enough for daily use.
Config options and defaults may change between minor releases, check the changelog when upgrading.

If you find a bug or if you're missing a feature, please [open an issue](https://github.com/prankstr/vibepanel/issues)! It's far from perfect yet.

### Compatibility

- **Compositors:** Hyprland, Niri, MangoWC/DWL. Sway support may be added based on demand.
- **Updates widget:** dnf and pacman/paru. More package managers planned.

## Quickstart

1. Install VibePanel:

   **Arch Linux (AUR):**

   ```sh
   yay -S vibepanel-bin
   ```

   **Fedora (COPR):**

   ```sh
   sudo dnf copr enable prankstr/vibepanel
   sudo dnf install vibepanel
   ```

   **Other distros:** Install [runtime dependencies](https://github.com/prankstr/vibepanel/wiki/Installation#runtime-dependencies), then:

   ```sh
   curl -LO https://github.com/prankstr/vibepanel/releases/latest/download/vibepanel-x86_64-unknown-linux-gnu
   install -Dm755 vibepanel-x86_64-unknown-linux-gnu ~/.local/bin/vibepanel
   ```

   Or [build from source](https://github.com/prankstr/vibepanel/wiki/Installation#from-source).

2. Run it:

   ```sh
   vibepanel
   ```

See the [Installation wiki](https://github.com/prankstr/vibepanel/wiki/Installation) for more information.

## Configuration

VibePanel doesn't require a config file to run, but if you want to customize anything, create a config at `~/.config/vibepanel/config.toml`:

```sh
touch ~/.config/vibepanel/config.toml
# or generate an example config
vibepanel --print-example-config > ~/.config/vibepanel/config.toml
```

Here's a minimal example:

```toml
[bar]
size = 32

[widgets]
left = ["workspaces", "window_title"]
right = ["quick_settings", "battery", "clock", "notifications"]

[theme]
mode = "dark"
accent = "#adabe0"
```

Changes hot-reload instantly. See the [Configuration wiki](https://github.com/prankstr/vibepanel/wiki/Configuration) for all options.

## Documentation

Full documentation lives in the [wiki](https://github.com/prankstr/vibepanel/wiki):

- [Installation](https://github.com/prankstr/vibepanel/wiki/Installation) - Dependencies, building, auto-start
- [Configuration](https://github.com/prankstr/vibepanel/wiki/Configuration) - All config options
- [Widgets](https://github.com/prankstr/vibepanel/wiki/Widgets) - Widget reference and per-widget options
- [Theming](https://github.com/prankstr/vibepanel/wiki/Theming) - Custom CSS styling
- [CSS Variables](https://github.com/prankstr/vibepanel/wiki/CSS-Variables) - Full CSS variable reference

## Vibe Code Disclaimer

As the name suggests, this project is mainly vibe code, i.e responsible AI-assisted development not just blindly accepting outputs. I'm not a Rust developer, nor was I particularly familiar with GTK when I started but it's been a great learning experience. Without AI, VibePanel wouldn't exist but it allowed me to create a bar I actually enjoy using so I'm thankful. I've done my due diligence to ensure the codebase is solid, but you have been warned :)

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests.

## License

MIT
