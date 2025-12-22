# VibePanel

![VibePanel](assets/screenshots/vibepanel_islands.png)

A GTK4 status bar for Wayland. Supports Hyprland, Niri, and DWL.

## Features

VibePanel aims to be a simple bar that just works and comes good-looking out of the box while being fully customizable.
Configure what you need, ignore what you don't. Changes hot-reload instantly.

- **Hot-reload** - config and style changes apply instantly
- **Multi-monitor support** - per-monitor configuration
- **Theming** - dark/light modes, custom accents, GTK theme integration, full CSS customization
- **OSD** - on-screen display for brightness and volume changes
- **CLI tools** - control brightness, volume, and idle inhibition from scripts
- **Widgets**
  - Workspaces - clickable indicators with tooltips
  - Window title - active window with app icon
  - Clock - configurable format with calendar popover
  - Battery - status with detailed popover and power profiles
  - Quick settings - audio, brightness, bluetooth, wifi, VPN, power profiles, idle inhibitor
  - System tray - XDG tray support
  - Notifications - notification center with Do Not Disturb
  - Updates - package update indicator (dnf, pacman/paru)
  - CPU & Memory - system resource monitors

## Screenshots

![Full bar](assets/screenshots/vibepanel_bar.png)
![Quick settings](assets/screenshots/vibepanel_qs.png)

## Status

VibePanel is in early 0.x development but should be stable enough for daily use.
Config options and defaults may change between minor releases, check the changelog when upgrading.

## Quickstart

1. Install runtime dependencies: Wayland, a supported compositor (Hyprland, Niri, MangoWC, DWL), PulseAudio/PipeWire, UPower, NetworkManager, BlueZ, GTK4.

2. Install VibePanel:

   - **Option A: Download a release binary** (recommended)

     Download the latest `vibepanel-<target>` from the GitHub Releases page and place it in your `$PATH`, e.g.:

     ```sh
     install -Dm755 vibepanel-x86_64-unknown-linux-gnu ~/.local/bin/vibepanel
     ```

   - **Option B: Build from source**

     ```sh
     git clone https://github.com/prankstr/vibepanel.git
     cd vibepanel
     cargo build --release
     install -Dm755 target/release/vibepanel ~/.local/bin/vibepanel
     ```

3. Create a config and run:

   ```sh
   mkdir -p ~/.config/vibepanel
   vibepanel --print-default-config > ~/.config/vibepanel/config.toml
   vibepanel &
   ```

## Configuration

Config lives at `~/.config/vibepanel/config.toml`. Here's a minimal example:

```toml
[widgets]
left = ["workspace", "window_title"]
right = ["quick_settings", "battery", "clock"]

[theme]
mode = "dark"
accent = "#adabe0"
```

See [docs/configuration.md](docs/configuration.md) for all options and [docs/css-variables.md](docs/css-variables.md) for styling.

## Vibe code Disclaimer

As the title suggests, this project is mainly vibe coded. While I've tried to do it responsibly, I'm not a Rust developer nor particularly familiar with GTK. Without AI, VibePanel wouldn't exist but it allowed me to create a bar I actually enjoy using so I'm thankful. I've done my due diligence to the best of my abilities to make sure the codebase is solid, but you have been warned :)

## Documentation

Configuration, theming, CLI usage, and compositor notes will be documented in the GitHub wiki.

For a raw reference of all config options and CSS variables, you can also check the files in `docs/` inside this repository.

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests.

## License

MIT
