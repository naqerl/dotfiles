#!/bin/bash

NVME_DEVICE="/dev/nvme0n1p1"
USER_HOME="/home/user"
MOUNT_POINT="$USER_HOME/media"
LOG_FILE="/var/log/nvme-media-mount.log"

log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | sudo tee -a "$LOG_FILE"
}

check_device() {
    if [ ! -b "$NVME_DEVICE" ]; then
        log_message "ERROR: NVMe device $NVME_DEVICE not found"
        exit 1
    fi
}

create_mount_point() {
    if [ ! -d "$MOUNT_POINT" ]; then
        mkdir -p "$MOUNT_POINT"
        log_message "Created mount point: $MOUNT_POINT"
    fi
}

mount_device() {
    if mountpoint -q "$MOUNT_POINT"; then
        log_message "Device already mounted at $MOUNT_POINT"
        exit 0
    fi
    
    # Try different filesystem types
    for fstype in ext4 ntfs exfat vfat; do
        if mount -t "$fstype" "$NVME_DEVICE" "$MOUNT_POINT" 2>/dev/null; then
            log_message "Successfully mounted $NVME_DEVICE to $MOUNT_POINT using $fstype"
            # Set proper ownership and permissions for user access (recursive)
            chown -R user:user "$MOUNT_POINT"
            chmod 755 "$MOUNT_POINT"
            exit 0
        fi
    done
    
    # If all mount attempts failed, try without specifying filesystem
    if mount "$NVME_DEVICE" "$MOUNT_POINT"; then
        log_message "Successfully mounted $NVME_DEVICE to $MOUNT_POINT (auto-detected filesystem)"
        # Set proper ownership and permissions for user access (recursive)
        chown -R user:user "$MOUNT_POINT"
        chmod 755 "$MOUNT_POINT"
    else
        log_message "ERROR: Failed to mount $NVME_DEVICE to $MOUNT_POINT"
        exit 1
    fi
}

main() {
    log_message "Starting NVMe media mount process"
    check_device
    create_mount_point
    mount_device
    log_message "NVMe media mount process completed successfully"
}

main "$@"