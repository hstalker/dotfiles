#!/usr/bin/env sh

assign_export BUNDLE_USER_CONFIG \
  "${XDG_CONFIG_HOME:-$HOME/.config}/bundle"
assign_export BUNDLE_USER_CACHE \
  "${XDG_CACHE_HOME:-$HOME/.cache}/bundle"
assign_export BUNDLE_USER_PLUGIN \
  "${XDG_DATA_HOME:-$HOME/.local/share}/bundle"

