#!/bin/bash

set -euo pipefail

echo "Finding orphaned XBPS caches..."
echo "================================"

# Get all installed packages
echo "Getting list of installed packages..."
installed_packages=$(xbps-query -l | awk '{print $2}' | cut -d'-' -f1- | sed 's/-[0-9].*$//')

# Get all cached packages
cache_dir="/var/cache/xbps"
if [[ ! -d "$cache_dir" ]]; then
    echo "Error: XBPS cache directory not found at $cache_dir"
    exit 1
fi

echo "Scanning cache directory: $cache_dir"
echo

# Find all cached package files
cached_files=$(find "$cache_dir" -name "*.xbps" 2>/dev/null || true)

if [[ -z "$cached_files" ]]; then
    echo "No cached packages found."
    exit 0
fi

orphaned_caches=()

# Check each cached file
while IFS= read -r cache_file; do
    if [[ -z "$cache_file" ]]; then
        continue
    fi
    
    # Extract package name from cache file
    basename_file=$(basename "$cache_file")
    # Remove .xbps extension and version info to get package name
    pkg_name=$(echo "$basename_file" | sed 's/\.xbps$//' | sed 's/-[0-9].*$//')
    
    # Check if this package is installed
    if ! echo "$installed_packages" | grep -q "^${pkg_name}$"; then
        orphaned_caches+=("$cache_file")
    fi
done <<< "$cached_files"

# Print results and delete orphaned caches
if [[ ${#orphaned_caches[@]} -eq 0 ]]; then
    echo "No orphaned caches found."
else
    echo "Found ${#orphaned_caches[@]} orphaned cache files:"
    echo
    for cache in "${orphaned_caches[@]}"; do
        file_size=$(du -h "$cache" | cut -f1)
        echo "  $cache ($file_size)"
    done
    
    echo
    total_size=$(du -ch "${orphaned_caches[@]}" | tail -n1 | cut -f1)
    echo "Total size of orphaned caches: $total_size"
    
    echo
    echo "Deleting orphaned caches..."
    for cache in "${orphaned_caches[@]}"; do
        if rm -f "$cache" 2>/dev/null; then
            echo "  Deleted: $(basename "$cache")"
        else
            echo "  Failed to delete: $cache (try with sudo)"
        fi
    done
    
    echo
    echo "Cleanup completed!"
fi