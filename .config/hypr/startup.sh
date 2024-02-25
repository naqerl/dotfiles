#!/usr/bin/env bash
hyprctl dispatch exec $HOME/.config/bin/notifications
hyprctl dispatch exec [workspace 1 silent] qutebrowser
# hyprctl dispatch exec [workspace 2 silent] 'kitty bash -c "tmux new -s main || tmux a"'
hyprctl dispatch workspace 2
