#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVICE_NAME="nvme-media"
RUNIT_SERVICE_DIR="/etc/sv/$SERVICE_NAME"

echo "Installing NVMe auto-mount system for runit..."

# Make scripts executable
chmod +x "$SCRIPT_DIR/mount-nvme-media.sh"
chmod +x "$SCRIPT_DIR/unmount-nvme-media.sh"
chmod +x "$SCRIPT_DIR/etc/sv/nvme-media/run"
chmod +x "$SCRIPT_DIR/etc/sv/nvme-media/finish"

# Create log file with proper permissions
sudo touch /var/log/nvme-media-mount.log
sudo chown $USER:$USER /var/log/nvme-media-mount.log

# Copy runit service to system directory
sudo cp -r "$SCRIPT_DIR/etc/sv/nvme-media" "$RUNIT_SERVICE_DIR"
sudo chown -R root:root "$RUNIT_SERVICE_DIR"

# Create service link to enable it
sudo ln -sf "$RUNIT_SERVICE_DIR" /var/service/

echo "Installation complete!"
echo ""
echo "To start the service now:"
echo "  sudo sv start $SERVICE_NAME"
echo ""
echo "To check status:"
echo "  sudo sv status $SERVICE_NAME"
echo ""
echo "To stop the service:"
echo "  sudo sv stop $SERVICE_NAME"
echo ""
echo "To view logs:"
echo "  tail -f /var/log/nvme-media-mount.log"
echo ""
echo "IMPORTANT: Edit etc/sv/nvme-media/run to set the correct NVMe device path"
echo "Current default: /dev/nvme0n1p1"
echo ""
echo "To find your NVMe device:"
echo "  lsblk | grep nvme"