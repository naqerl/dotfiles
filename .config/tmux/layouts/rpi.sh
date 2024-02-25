#!/usr/bin/env bash
# IGNORE_PATH
session_name="rpi"
tmux send-keys -t "$session_name":1 "ssh pi@raspberrypi.local" Enter
