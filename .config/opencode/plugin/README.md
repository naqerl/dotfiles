# OpenCode Notification Plugin

Desktop notifications for OpenCode events.

## Features

Sends `notify-send` desktop notifications for:

- **Session Idle**: When OpenCode finishes work and is ready for your next request
- **Session Error**: When an error occurs during a session

Each notification includes the project name from the current directory.

## Requirements

- `notify-send` command (usually part of `libnotify` package on Linux)
- System notification daemon running (e.g., dunst, mako, notification-daemon)

## Installation

The plugin is automatically loaded from `~/.config/opencode/plugin/notify.js`.

No additional configuration needed.

## Notification Details

| Event | Urgency | Duration | Message |
|-------|---------|----------|---------|
| Session Idle | Normal | 5s | "Session completed! Ready for your next request." |
| Session Error | Critical | 10s | "Session error occurred. Check the terminal for details." |

## Customization

Edit `~/.config/opencode/plugin/notify.js` to customize:
- Notification messages
- Urgency levels (`-u low/normal/critical`)
- Display duration (`-t milliseconds`)
- Which events trigger notifications
