# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases setup
alias ll="exa --long --header --icons"
alias g="git"
alias d="docker"
alias gr="cd (gitroot.sh)"
alias rm="trash"
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Env setup
. "$HOME/.cargo/env"

# Promt setup
__main() {
    local major="${BASH_VERSINFO[0]}"
    local minor="${BASH_VERSINFO[1]}"

    if ((major > 4)) || { ((major == 4)) && ((minor >= 1)); }; then
        source <(/usr/bin/starship init bash --print-full-init)
    else
        source /dev/stdin <<<"$(/usr/bin/starship init bash --print-full-init)"
    fi
}
__main
unset -f __main

# Emacs vterm setup
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	echo "Vterm profile"

	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh

	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;$(basename $PWD)\007"'

	vterm_cmd() {
	    local vterm_elisp
	    vterm_elisp=""
	    while [ $# -gt 0 ]; do
		vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
		shift
	    done
	    vterm_printf "51;E$vterm_elisp"
	}

	find_file() {
	    vterm_cmd find-file "$(realpath "${@:-.}")"
	}

	say() {
	    vterm_cmd message "%s" "$*"
	}
fi
