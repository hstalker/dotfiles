#!/usr/bin/env sh

silence_output assert_directory "${XDG_CACHE_HOME:-$HOME/.cache}/less"

# Set less as the default pager
assign_export PAGER 'less'

# Force less to place & find its files in sensible places
assign_export LESSHISTFILE \
  "${XDG_CACHE_HOME:-$HOME/.cache}/less/history"
assign_export LESSKEY \
  "${XDG_CONFIG_HOME:-$HOME/.config}/less/lesskey"

# Enable ANSI color code interpretation by default for all less invocations
assign_export LESS "-R"
