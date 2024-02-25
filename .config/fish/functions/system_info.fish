function system_info
    cd "$HOME"
    fd --type d --max-depth 1 --hidden --exec du -sh {} | sort -rh | head -n 10
end
