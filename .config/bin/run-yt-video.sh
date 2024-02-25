#!/usr/bin/env bash

# Usage (with copied yt link):
# ./run-yt-video.sh		-> Opens player with video
# ./run-yt-video.sh --no-video	-> Only background music

# Setup:
# sudo apt-get install -y mpv python3-pip mpv-mpris libnotify-bin
# pip install yt-dlp
#
# Extensions from showing status:
# 1. https://extensions.gnome.org/extension/4928/mpris-label/
# 2. https://extensions.gnome.org/extension/4470/media-controls/

url=$(xclip -o)
if [[ "$url" != http* ]]; then
    notify-send "Not an url"
    exit 1
fi

mpv_args=${1:-""}

killall mpv
PATH=$PATH:$HOME/.local/bin
notify-send "Starting MPV"
mpv "$mpv_args" "$url"
