#!/usr/bin/env bash
# IGNORE_PATH

session_name="music"

tmux send-keys -t "$session_name":1 'cd "$HOME/Music"' Enter
tmux send-keys -t "$session_name":1 'make fresh' Enter
tmux send-keys -t "$session_name":1 'udisksctl mount -b /dev/sdb1' Enter
tmux send-keys -t "$session_name":1 'nvim -c "Oil $HOME/Music" -c "vertical Oil /run/media/suzu/MUSIC" -c "lua vim.notify(vim.fn.system(\"du -sh /home/suzu/Music\", true))" && udisksctl unmount -b /dev/sdb1 && notify-send "Player could be ejected"' Enter
