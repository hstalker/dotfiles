#!/usr/bin/env sh

# This causes dumb issues with client connection to this box
# assign_export XAUTHORITY "${XDG_RUNTIME_DIR}/Xauthority"
assign_export XINITRC "${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc"
assign_export XSERVERRC "${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xserverrc"
assign_export XCOMPOSEFILE "${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xcompose"
assign_export XCOMPOSECACHE "${XDG_CACHE_HOME:-$HOME/.cache}/xorg/xcompose"

# ICEAuthority
assign_export ICEAUTHORITY "${XDG_CACHE_HOME:-$HOME/.cache}/ICEauthority"

