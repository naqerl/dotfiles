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
	git \ 
	gptfdisk \ 
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

# Pipewire setup
mkdir -p /etc/pipewire/pipewire.conf.d
ln -s /usr/share/examples/wireplumber/10-wireplumber.conf /etc/pipewire/pipewire.conf.d/

sudo mkdir -p /etc/pipewire/pipewire.conf.d
sudo ln -s /usr/share/examples/pipewire/20-pipewire-pulse.conf /etc/pipewire/pipewire.conf.d/

sudo mkdir -p /etc/alsa/conf.d
sudo ln -s /usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d
sudo ln -s /usr/share/alsa/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d

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
