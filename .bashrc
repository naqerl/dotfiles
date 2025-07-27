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
      exit 0
  fi
  echo "Jumped to project at ~/$dir"
  cd "$dir"
}

# Aliases
alias g='git'
alias p='podman'
alias xi='sudo xbps-install'
alias xq='xbps-query -Rs'
alias hexec='hyprctl dispatch exec'
