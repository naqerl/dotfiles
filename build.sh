set -e

git pull origin main

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"
rustup default stable

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
        fish \
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
        stow

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
        vesktop-bin

paru -S ttf-iosevka-nerd \
        ttf-iosevka-lyte-nerd-font \
        ttf-liberation \
        ttf-opensans

paru -S catppuccin-gtk-theme-frappe \
        catppuccin-cursors-frappe \
        papirus-icon-theme

git clone https://github.com/abba23/spotify-adblock
cd spotify-adblock
make
sudo make install
cd ..
sudo rm -r spotify-adblock

bash .config/bin/spicetify-install.sh
export PATH="$PATH:$HOME/.spicetify"
spicetify config current_theme catppuccin
spicetify config color_scheme frappe
spicetify config inject_css 1 inject_theme_js 1 replace_colors 1 overwrite_assets 1
spicetify backup apply
sudo rm -r ~/.spicetify install.log

curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
fisher update
chsh -s `which fish`

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

cd "$HOME/dotfiles" && stow .

paru -S python-pipx \
     python-poetry

pipx install pyright

rustup component add rust-analyzer

paru -S docker \
        docker-buildx \
        docker-compose
