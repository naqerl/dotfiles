#!/bin/sh
WARNING_TEMP=${WARNING_TEMP:-55}
temp=$(sensors | rg -i cpu | awk '{print $2}' | sed 's/[^0-9.]*//g' | awk '{printf "%.0f", $1}')
if [ "$temp" -gt "$WARNING_TEMP" ]; then
	echo "#[fg=orange]#[bg=orange,fg=#000000]  #[bg=colour234,fg=white] $temp°C #[fg=colour234,bg=default]"
fi
