set -e

sudo pacman -S --needed base-devel
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si
cd .. && rm -r paru

paru -S hyprland \
        hyprpaper \
        hyprpicker \
        xdg-desktop-portal-hyprland \
        eww-wayland \
        fish \
        starship \
        tiramisu \
        ripgrep \
        fd \
        jq xq \
        git \
        wl-clipboard \
        libnotify \
        pipewire pipewire-alsa pipewire-pulse pipewire-jack pipewire-audio \
        feh \
        gim \
        slurp \
        socat \
        pass \
        stow \
        --noconfirm

paru -S kitty \
        qutebrowser \
        mpv mpv-mpris \
        telegram-desktop \
        neovim \
        tmux \
        thunar thunar-archive-plugin \
        udisks \
        batsignal \
        fuzzel \
        btop \
        pavucontrol \
        networkmanager \
        blueman \
        spotify-wayland \
        vesktop-bin \ 
        --noconfirm

paru -S ttf-iosevka-nerd \
        ttf-iosevka-lyte-nerd-font \
        ttf-liberation \
        ttf-opensans \
        --noconfirm

paru -S catppuccin-gtk-theme-frappe \
        catppuccin-cursors-frappe \
        papirus-icon-theme \
        --noconfirm

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
     python-poetry \
     --noconfirm

pipx install pyright

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"

rustup component add rust-analyzer

paru -S docker \
        docker-buildx \
        docker-compose \
        --noconfirm
