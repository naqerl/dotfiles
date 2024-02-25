#!/usr/bin/env bash

set -e

profile=$(choose-google-profile.py)

if [[ -z "$profile" ]]; then
	exit 1
fi

google-chrome-stable --profile-directory="$profile"
