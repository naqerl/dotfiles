#!/usr/bin/env bash
tmuxinator list | tail -n +2 | sed 's/ \+/\n/g'
