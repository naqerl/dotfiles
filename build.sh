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

rustup component add rust-analyzer

paru -S docker \
        docker-buildx \
        docker-compose \
        --noconfirm
