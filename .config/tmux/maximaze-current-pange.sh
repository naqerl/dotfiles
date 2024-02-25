#!/usr/bin/env bash
status_var_name="TMUX_PANE_MAXIMIZED"
is_maximized=$(tmux display-message -p "#{$status_var_name}")

if [[ -z "$is_maximized" ]]; then
	pane_idx=$(tmux display-message -p "#{pane_index}")
	tmux set-environment -g "$status_var_name" "#[fg=cyan]#[bg=cyan,fg=black]  #[bg=#292c3d,fg=white] $pane_idx #[fg=#292c3d,bg=default]"
else
	tmux set-environment -g "$status_var_name" ""
fi

tmux resize-pane -Z
