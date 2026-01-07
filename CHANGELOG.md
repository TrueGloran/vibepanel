# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-01-07

### Added

- Support for markup in notifications, allowing rich text formatting
- Calendar week header display
- Settings option to disable calendar weeks

### Changed

- CI optimization improvements

## [0.1.1] - 2024-12-30

### Fixed

- Notification text now truncates on character boundaries instead of bytes, preventing multibyte characters (e.g., åäö) from being split
- Password input in WiFi quick settings panel
- Truncation of subtitles in toggle cards

## [0.1.0] - Initial Release

- Initial release of vibepanel
