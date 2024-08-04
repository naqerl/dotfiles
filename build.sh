set -ex

if [[ -z $(command -v rustup) ]]; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    source "$HOME/.cargo/env"
    rustup default stable
fi

if [[ -z $(command -v paru) ]]; then
    sudo pacman -S --needed base-devel
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si
    cd .. && rm -r paru
fi

paru -S hyprland \
        hyprpaper \
        hyprpicker \
        xdg-desktop-portal-hyprland \
        eww-tray-wayland-git \
        fish fisher \
        starship \
        tiramisu-git \
        ripgrep \
        fd \
        jq xq \
        git \
        wl-clipboard \
        libnotify \
        pipewire pipewire-alsa pipewire-pulse pipewire-jack pipewire-audio \
        feh \
        grim \
        slurp \
        socat \
        pass \
        stow \
        --needed --noconfirm

paru -S kitty \
        qutebrowser \
        mpv mpv-mpris \
        telegram-desktop \
        neovim \
        tmux \
        thunar thunar-archive-plugin \
        udisks2 \
        batsignal \
        fuzzel \
        btop \
        pavucontrol \
        networkmanager \
        blueman \
        spotify-wayland \
        vesktop-bin \
        --needed --noconfirm

paru -S ttf-iosevka-nerd \
        ttf-iosevka-lyte-nerd-font \
        ttf-liberation \
        ttf-opensans \
        --needed --noconfirm

paru -S catppuccin-gtk-theme-frappe \
        catppuccin-cursors-frappe \
        papirus-icon-theme \
        --needed --noconfirm

if [[ "$1" == spotify ]]; then
    if [[ ! -d spotify-adblock ]]; then
        git clone https://github.com/abba23/spotify-adblock
    fi

    if [[ ! -f /usr/local/lib/spotify-adblock.so ]]; then
        cd spotify-adblock
        make
        sudo make install
        cd ..
        sudo rm -r spotify-adblock
    fi
fi

if [[ "$1" == spotify ]]; then
    bash .config/spicetify/spicetify-install.sh
    export PATH="$PATH:$HOME/.spicetify"
    spicetify config current_theme catppuccin
    spicetify config color_scheme frappe
    spicetify config inject_css 1 inject_theme_js 1 replace_colors 1 overwrite_assets 1
    spicetify backup apply
    sudo rm -r ~/.spicetify install.log
fi

chsh -s $(which fish)

if [[ ! -d "$HOME/.tmux/plugins/tmp" ]]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

cd "$HOME/dotfiles" && stow . --adopt

paru -S python-pipx \
     python-poetry \
     --needed --noconfirm

pipx install pyright

rustup component add rust-analyzer

paru -S docker \
        docker-buildx \
        docker-compose \
        --needed --noconfirm
