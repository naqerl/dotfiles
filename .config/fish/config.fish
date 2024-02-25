 if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
     Hyprland
 end

function fish_greeting
end

function fish_user_key_bindings
    fish_default_key_bindings -M insert
    fish_vi_key_bindings --no-erase insert
end

if status is-interactive
    if test $TERM != 'dumb'
        function starship_transient_prompt_func
            starship module time
        end

        set -Ux STARSHIP_CONFIG $HOME/.config/starship.toml

        starship init fish | source
        enable_transience
        direnv hook fish | source

        nvm use lts 1>/dev/null
    end

    alias ll 'exa --long --header --icons'
    alias g git
    alias d docker
    alias gr 'cd (gitroot.sh)'
    alias rm trash

    function gpt
        sgpt --model gpt-3.5-turbo $argv
    end

    fish_add_path "$HOME/.local/bin/"
    fish_add_path "$HOME/.config/bin/"
    fish_add_path "$HOME/.cargo/bin/"

    envsource "$HOME/.config/fish/.env"
end

function try_xq
    set query '. as $line | try (fromjson) catch $line'
    if count $argv > 0
        set query "$query | $argv"
    end
    xq -R "$query"
end
