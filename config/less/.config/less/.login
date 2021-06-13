#!/usr/bin/env sh

silence_output assert_directory "${XDG_CACHE_HOME:-$HOME/.cache}/less"

# Force less to place & find its files in sensible places
LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/less/history"
LESSKEY="${XDG_CONFIG_HOME:-$HOME/.config}/less/lesskey"

# Enable ANSI color code interpretation by default for all less invocations
LESS="-R"
