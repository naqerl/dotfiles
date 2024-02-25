#!/usr/bin/env bash
set -e

fish -i -c 'read -P "GPT ❯ " prompt && echo && sgpt --model gpt-3.5-turbo $prompt'

echo
cols=$(tput cols)
for ((i=0; i<cols; i++)); do
	echo -n "─"
done
echo

# while true; do sleep 5; done
read -n 1 -r -s
