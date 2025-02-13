git config --global core.excludesfile ~/dotfiles/.system_wide_gitignore

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
        direnv \
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
        --needed

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
        swaybg \
        --needed

paru -S ttf-iosevka-nerd \
        ttf-iosevka-lyte-nerd-font \
        ttf-liberation \
        ttf-opensans \
        --needed

paru -S catppuccin-gtk-theme-frappe \
        catppuccin-cursors-frappe \
        papirus-icon-theme \
        --needed

chsh -s $(which fish)
fish -c 'curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher'
fish -c 'fisher install jorgebucaran/nvm.fish'
fish -c 'nvm install lts'

if [[ ! -d "$HOME/.tmux/plugins/tpm" ]]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

cd "$HOME/dotfiles" && stow . --adopt

paru -S python-pipx \
     python-poetry \
     --needed

pipx install pyright

rustup component add rust-analyzer

paru -S docker \
        docker-buildx \
        docker-compose \
        --needed
