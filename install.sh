# Install packages
sudo xbps-install \ 
	acpi \ 
	base-devel \ 
	base-system \ 
	bash-completion \ 
	brltty \ 
	btop \ 
	chrony \ 
	cryptsetup \ 
	curl \ 
	delta \
	elogind \ 
	espeakup \ 
	fd \ 
	firefox \ 
	fzf \ 
	ghostty \ 
	git \ 
	gptfdisk \ 
	greetd \ 
	grim \ 
	hyprland \ 
	hyprland-qtutils \ 
	libnotify \
	linux \ 
	lvm2 \ 
	lz4 \ 
	make \ 
	mdadm \ 
	mesa \ 
	mesa-dri \ 
	mpv \ 
	neovim \ 
	nerd-fonts-ttf \ 
	noto-fonts-emoji \ 
	noto-fonts-ttf \ 
	pass \ 
	pinentry-gnome \ 
	pipewire \ 
	podman \ 
	polkit \ 
	refind \ 
	ripgrep \ 
	slurp \ 
	swappy \ 
	telegram-desktop \ 
	tmux \ 
	unzip \
	void-docs-browse \ 
	void-live-audio \ 
	wget \ 
	wireplumber \ 
	wl-clipboard \ 
	wmenu \ 
	wtype \ 
	xdg-desktop-portal-hyprland \ 
	xdg-utils \

# Enable services
sudo ln -s /etc/sv/dbus /var/service
sudo ln -s /etc/sv/greetd /var/service
sudo ln -s /etc/sv/polkitd /var/service
sudo mkdir -p /etc/pipewire/pipewire.conf.d
sudo ln -s /usr/share/examples/wireplumber/10-wireplumber.conf /etc/pipewire/pipewire.conf.d/
sudo ln -s /usr/share/examples/pipewire/20-pipewire-pulse.conf /etc/pipewire/pipewire.conf.d/

# Install dotfiles
git clone https://github.com/scipunch/dotfiles $HOME/dotfiles
mkdir -p $HOME/.config
cd $HOME/dotfiles
ln -s $PWD/.config/hypr $HOME/.config/hypr

# GoogleDot cursor
cursor_file="GoogleDot-Black.tar.gz"
wget "https://github.com/ful1e5/Google_Cursor/releases/download/v2.0.0/${cursor_file}"
tar -xvf "$cursor_file"
rm "$cursor_file"
sudo mv GoogleDot-* /usr/share/icons/

# Opencode
curl -fsSL https://opencode.ai/install | bash

# pnpm
curl -fsSL https://get.pnpm.io/install.sh | sh -

# hyprnotify
wget -O /tmp/hyprnotify.zip https://github.com/codelif/hyprnotify/releases/download/v0.8.0/hyprnotify.zip
unzip /tmp/hyprnotify.zip -d $HOME/.local/bin/
chmod +x $HOME/.local/bin/hyprnotify
