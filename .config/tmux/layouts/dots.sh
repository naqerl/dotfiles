#!/usr/bin/env bash
# IGNORE_PATH
session_name="dots"
tmux send-keys -t "$session_name":1 "cd $HOME/.config; nvim" Enter
