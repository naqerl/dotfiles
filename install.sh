#!/usr/bin/env bash

LINKS_FILE="$PWD/links.conf"

# Hyprland repository (https://wiki.hypr.land/0.42.0/Getting-Started/Installation/)
echo "repository=https://raw.githubusercontent.com/Makrennel/hyprland-void/repository-x86_64-glibc" > /etc/xbps.d/hyprland-void.conf

# Install packages
sudo xbps-install -S \ 
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
	dunst \
	elogind \ 
	espeakup \ 
	fd \ 
	firefox \ 
	fzf \ 
	git \ 
	greetd \ 
	grim \ 
	hyprland \ 
	hyprland-qtutils \ 
	hyprpicker \
	libnotify \
	libspa-bluetooth \ # pipewire bluetooth
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
	qBittorrent \
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
	kitty \
	snooze \

# Pipewire setup
mkdir -p /etc/pipewire/pipewire.conf.d
ln -s /usr/share/examples/wireplumber/10-wireplumber.conf /etc/pipewire/pipewire.conf.d/
sudo mkdir -p /etc/pipewire/pipewire.conf.d
sudo ln -s /usr/share/examples/pipewire/20-pipewire-pulse.conf /etc/pipewire/pipewire.conf.d/
sudo mkdir -p /etc/alsa/conf.d
sudo ln -s /usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d
sudo ln -s /usr/share/alsa/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d

# Install dotfiles
mkdir -p $HOME/.config
while read -r src target; do
	# NOTE: Suppose there will be only links made by me
	if [[ -e "$target" && ! -L "$target" ]]; then
		cmd="mv '$target' '$target.back'"
		if [[ "$target" =~ ^/ ]]; then
			cmd="sudo $cmd"
		fi
		echo "executing $cmd"
	fi
	if [[ ! $src =~ ^/ ]]; then
		# Link's "to" should be absolute in this case
		src="$PWD/$src"
	fi
	cmd="ln -s '$src' '$target'"
	if [[ "$target" =~ ^/ ]]; then
		cmd="sudo $cmd"
	fi
	echo "executing $cmd"
done < "$LINKS_FILE"

# GoogleDot cursor
cursor_file="GoogleDot-Black.tar.gz"
wget "https://github.com/ful1e5/Google_Cursor/releases/download/v2.0.0/${cursor_file}"
tar -xvf "$cursor_file"
rm "$cursor_file"
sudo mv GoogleDot-* /usr/share/icons/

# pnpm
curl -fsSL https://get.pnpm.io/install.sh | sh -

# Golang
gotar="go1.24.5.linux-amd64.tar.gz"
wget "https://go.dev/dl/$gotar" -O "/tmp/$gotar"
sudo rm -rf /usr/local/go && tar -C /usr/local -xzf "/tmp/$gotar"

# qBittorrent plugins
plugins_path=$HOME/.local/share/qBittorrent/nova3/engines
source="https://raw.githubusercontent.com/BurningMop/qBittorrent-Search-Plugins/main"
plugins=(
	"bitsearch.py"
	"calidadtorrent.py"
	"divxtotal.py"
	"dontorrent.py"
	"esmeraldatorrent.py"
	"naranjatorrent.py"
	"pediatorrent.py"
	"solidtorrents.py"
	"therarbg.py"
	"tomadivx.py"
	"torrenflix.py"
	"torrentdownloads.py"
	"traht.py"
)
mkdir -p $pugins_path
cd $plugins_path
for plugin in "${plugins[@]}"; do
	wget "${source}/${plugin}"
done
