#!/bin/env bash
session_to_kill="$1"
current_session_name=$(tmux display-message -p "#{session_name}")

if [[ "$session_to_kill" == "$current_session_name" ]]; then
	tmux switch-client -n
fi

tmux kill-session -t "$session_to_kill"
