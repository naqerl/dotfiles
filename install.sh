# Install packages
sudo xbps-install ansible-core base-system bluez chromium chrony cryptsetup curl dbus dialog droidcam-obs-plugin elogind emacs-pgtk fd font-iosevka foot git gnupg greetd grim grub-i386-efi grub-x86_64-efi hyprland hyprland-qtutils hyprpicker  libnotify libspa-bluetooth linux lvm2 make mako mdadm mesa-dri mesa-vulkan-radeon mitmproxy mpv nodejs noto-fonts-cjk noto-fonts-emoji noto-fonts-ttf nwg-look obs pass pipewire podman polkit pulsemixer python3 qbittorrent ripgrep rsync seatd slurp swappy telegram-desktop tmux unzip v4l2loopback void-docs-browse void-live-audio vulkan-loader wget wireplumber wtype xdg-desktop-portal-hyprland xorg-server-xwayland yt-dlp

# Enable services
sudo ln -s /etc/sv/dbus /var/service
sudo ln -s /etc/sv/greetd /var/service
sudo ln -s /etc/sv/polkitd /var/service
sudo ln -s /etc/sv/seatd /var/service
sudo mkdir -p /etc/pipewire/pipewire.conf.d
sudo ln -s /usr/share/examples/wireplumber/10-wireplumber.conf /etc/pipewire/pipewire.conf.d/
sudo ln -s /usr/share/examples/pipewire/20-pipewire-pulse.conf /etc/pipewire/pipewire.conf.d/

# Install dotfiles
git clone https://github.com/scipunch/dotfiles $HOME/dotfiles
mkdir -p $HOME/.config
cd $HOME/dotfiles
ln -s $PWD/.config/hypr $HOME/.config/hypr
ln -s $PWD/.emacs.d $HOME/.emacs.d
