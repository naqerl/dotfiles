#!/usr/bin/env bash

set_value() {
  key="$1"
  value="$2"
  eval "assoc_$key='$value'"
}

get_value() {
  key="$1"
  eval "echo \${assoc_$key}"
}

handle() {
  case $1 in
    openwindow*) echo "New window $1" ;;
  esac
}

set_value 'emacs' '1'
set_value 'zen-alpha' '2'

socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do handle "$line"; done

# hyprctl dispatch exec 'emacsclient --create-frame --alternate-editor=""'
# hyprctl dispatch exec  'zen-browser'
