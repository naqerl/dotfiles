# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto --hyperlink=auto'
PS1='[\u@\h \W]\$ '

# Use bash-completion, if available
[[ -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# Use fzf completion, if available
[[ -f $(which fzf) ]] && \
		eval "$(fzf --bash)"

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

# Save each command to history immediately
export HISTCONTROL=ignoredups:erasedups
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

export FZF_DEFAULT_OPTS=--reverse
