# NVMe Auto-Mount System (Runit)

This system automatically mounts your NVMe disk to `~/media` using runit services.

## Files Created

- `mount-nvme-media.sh` - Main mounting script (standalone use)
- `unmount-nvme-media.sh` - Unmounting script (standalone use)
- `etc/sv/nvme-media/run` - Runit service run script
- `etc/sv/nvme-media/finish` - Runit service finish script  
- `install-nvme-mount.sh` - Installation script

## Setup Instructions

1. **Find your NVMe device:**
   ```bash
   lsblk | grep nvme
   sudo fdisk -l | grep nvme
   ```

2. **Edit the runit service:**
   ```bash
   nano etc/sv/nvme-media/run
   ```
   Update the `NVME_DEVICE` variable with your actual device path (e.g., `/dev/nvme0n1p1`)

3. **Install the system:**
   ```bash
   ./install-nvme-mount.sh
   ```

4. **Start the service:**
   ```bash
   sudo sv start nvme-media
   ```

## Runit Service Management

**Start service:**
```bash
sudo sv start nvme-media
```

**Stop service:**
```bash
sudo sv stop nvme-media
```

**Check service status:**
```bash
sudo sv status nvme-media
```

**Restart service:**
```bash
sudo sv restart nvme-media
```

**Disable service:**
```bash
sudo sv down nvme-media
sudo rm /var/service/nvme-media
```

## Manual Operations

**Mount manually:**
```bash
./mount-nvme-media.sh
```

**Unmount manually:**
```bash
./unmount-nvme-media.sh
```

**Check if mounted:**
```bash
mountpoint ~/media
```

## Monitoring

**View mount logs:**
```bash
tail -f /var/log/nvme-media-mount.log
```

**Watch service in real-time:**
```bash
sudo sv status nvme-media
watch -n 2 'sudo sv status nvme-media'
```

## How It Works

The runit service:
- Runs continuously and monitors the mount point
- Automatically mounts the NVMe device when detected
- Re-mounts if the mount is lost
- Supports multiple filesystem types (ext4, ntfs, exfat, vfat)
- Gracefully unmounts when the service is stopped

## Troubleshooting

- Ensure the NVMe device path is correct in `etc/sv/nvme-media/run`
- Check if the device is already mounted elsewhere: `mount | grep nvme`
- Verify filesystem type is supported (ext4, ntfs, exfat, vfat)
- Check device permissions and ownership
- Service logs are in `/var/log/nvme-media-mount.log`