#!/bin/bash

USER_HOME="/home/user"
MOUNT_POINT="$USER_HOME/media"
LOG_FILE="/var/log/nvme-media-mount.log"

log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | sudo tee -a "$LOG_FILE"
}

unmount_device() {
    if ! mountpoint -q "$MOUNT_POINT"; then
        log_message "Device not mounted at $MOUNT_POINT"
        exit 0
    fi
    
    # Try to unmount gracefully
    if umount "$MOUNT_POINT"; then
        log_message "Successfully unmounted $MOUNT_POINT"
    else
        log_message "WARNING: Graceful unmount failed, attempting lazy unmount"
        if umount -l "$MOUNT_POINT"; then
            log_message "Successfully performed lazy unmount of $MOUNT_POINT"
        else
            log_message "ERROR: Failed to unmount $MOUNT_POINT"
            exit 1
        fi
    fi
}

main() {
    log_message "Starting NVMe media unmount process"
    unmount_device
    log_message "NVMe media unmount process completed"
}

main "$@"