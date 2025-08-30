# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto --hyperlink=auto'
PS1='[\u@\h \W]\$ '

# pnpm
export PNPM_HOME="/home/user/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Use bash-completion, if available
[[ -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# Use fzf completion, if available
[[ -f $(which fzf) ]] && \
		eval "$(fzf --bash)"


pj() {
  cd $HOME
  dir=$(find code/ -maxdepth 5 -type d -name .git | xargs -I{} dirname {} | fzf)
  if [[ -z $dir ]]; then
      cd -
      exit 1
  fi
  name=$(basename "$dir")
  switch_tmux() {
    if [[ -n "$TMUX" ]]; then
      tmux switch-client -t "$1"
    else
      tmux attach -s "$1"
    fi
  }
  if ! tmux ls -F '#{session_name}' | grep -q "$name"; then
    tmux new -d -s "$name" -c "$dir" 'nvim'
    tmux new-window -t "$name" -n "opencode" -c "$dir" 'opencode'
    tmux new-window -t "$name" -n "shell" -c "$dir"
    tmux select-window -t "$name":0
  fi
  switch_tmux "$name"
}

p() {
  if [[ -z "$1" ]]; then
    pj
  else
    podman $@
  fi
}

wpe() {
  bash -c "source <(pass show personal-env) && $@"
}

# Aliases
alias g='git'
alias xi='sudo xbps-install'
alias xq='xbps-query -Rs'
alias hexec='hyprctl dispatch exec'

# opencode
export PATH=/home/user/.opencode/bin:$PATH

# Turso
export PATH="$PATH:/home/user/.turso"
