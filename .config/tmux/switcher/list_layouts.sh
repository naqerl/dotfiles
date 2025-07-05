#!/usr/bin/env bash
set -e 

LAYOUTS_DIR="$HOME/.config/tmux/layouts/"

cd "$LAYOUTS_DIR"

fd -t f \
	| xargs -I {} bash -c 'rg -q "^# IGNORE_PATH$" "{}" && echo "{}"' \
	| rg -v '^$' \
	| sed 's/\.sh$//'
