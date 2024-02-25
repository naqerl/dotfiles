#!/usr/bin/env bash
# IGNORE_PATH
session_name="haas"
base_path="$HOME/code/work/haas"
tmux send-keys -t "$session_name":1 "cd $base_path/server && docker compose up -d && notify-send 'Haas server started' && docker compose logs --tail=100 -f" Enter

tmux new-window -t "$session_name":2 -c "$base_path/haaslib/main" -n "Lib Editor"
tmux send-keys -t "$session_name":2 'nvim src/lib.rs' Enter

tmux new-window -t "$session_name":3 -c "$base_path/haas-client-v4/main" -n "Client Editor"
tmux send-keys -t "$session_name":3 'nvim src/main.rs' Enter

tmux new-window -t "$session_name":4 -c "$base_path/haaspylib/main" -n "Py binds Editor"
tmux send-keys -t "$session_name":4 'nvim src/main.rs' Enter
