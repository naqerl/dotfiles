# OpenCode Notification Plugin

Desktop notifications for OpenCode events.

## Features

Sends `notify-send` desktop notifications for:

- **Session Idle**: When OpenCode finishes work and is ready for your next request
- **Session Error**: When an error occurs during a session  
- **Permission Required**: When OpenCode needs your permission to proceed (only once per unique permission)

Each notification includes the project name from the current directory.

The plugin tracks permissions to avoid duplicate notifications and clears tracking when sessions become idle.

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
| Permission Required | Normal | 8s | "Permission required - check the terminal" |

## Customization

Edit `~/.config/opencode/plugin/notify.js` to customize:
- Notification messages
- Urgency levels (`-u low/normal/critical`)
- Display duration (`-t milliseconds`)
- Which events trigger notifications
