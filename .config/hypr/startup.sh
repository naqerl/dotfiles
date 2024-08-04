hyprctl dispatch exec $HOME/.config/bin/notifications
hyprctl dispatch exec [workspace 1 silent] qutebrowser
hyprctl dispatch exec emacs --daemon && notify-send "Emacs started"
hyprctl dispatch workspace 2
