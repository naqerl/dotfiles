#!/usr/bin/env bash
set -e

PROJECTS_BASE_DIR="$HOME/code"
LIST_SESSIONS="${1:-$HOME/.config/tmux/switcher/list_sessions.sh}"
LIST_PROJECTS="${2:-$HOME/.config/tmux/switcher/list_projects.sh}"
LIST_LAYOUTS="${3:-$HOME/.config/tmux/switcher/list_layouts.sh}"
KILL_SESSION="${4:-$HOME/.config/tmux/switcher/kill_session.sh}"
LIST_DIRECTORIES="${5:-$HOME/.config/tmux/switcher/list_directories.sh}"

select_session() {
    header="┌─────────────┐
│   Switcher │
├─────────────┴───────────────────────────────────────────────────────────┐
│ List:	  Ctrl + [S]essions   | pr[O]jects   | [L]ayouts   | [F]ind   │
│ Action: Ctrl + [D]elete      | n[E]w                                    │
└─────────────────────────────────────────────────────────────────────────┘
"

"$HOME/.config/tmux/switcher/list_sessions.sh" | fzf --reverse \
	--prompt '   ' \
	--header-first \
	--color=bg:18 \
	--header "$header" \
	--bind "ctrl-s:change-prompt(   )+reload($LIST_SESSIONS)" \
	--bind "ctrl-o:change-prompt(   )+reload($LIST_PROJECTS $PROJECTS_BASE_DIR)" \
	--bind "ctrl-l:change-prompt(   )+reload($LIST_LAYOUTS)" \
	--bind "ctrl-f:change-prompt(   )+reload($LIST_DIRECTORIES)" \
	--bind "ctrl-e:become(echo {q})" \
	--bind "ctrl-d:reload($KILL_SESSION {}; $LIST_SESSIONS)+clear-query"
}

session_name=$(select_session)

# Exit here if nothing was selected
if [[ -z "$session_name" ]]; then
    exit 1
fi

session_path=$HOME

if [[ -d "$PROJECTS_BASE_DIR/$session_name" ]]; then
    session_path="$PROJECTS_BASE_DIR/$session_name"
    session_name=$(basename "$session_name")
fi

session_was_created=0

if ! tmux list-sessions | awk '{print $1}' | rg "$session_name\:\$"; then
    echo "Session wasn't created"
    tmux new-session -d -s "$session_name" -c "$session_path"
    session_was_created=1
fi

tmux switch-client -t "$session_name"

# Run layout if exists
if [[ $session_was_created == 1 ]]; then
    base_layout_path="$HOME/.config/tmux/layouts"
    layout=$(cd "$base_layout_path" && fd "$session_name.sh")

    if [[ -z "$layout" ]]; then
	exit 0
    fi

    "$base_layout_path/$layout"
fi
