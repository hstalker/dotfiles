#!/usr/bin/env sh

# This causes dumb issues with client connection to this box
# export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc"
export XSERVERRC="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xserverrc"
export XCOMPOSEFILE="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xcompose"
export XCOMPOSECACHE="${XDG_CACHE_HOME:-$HOME/.cache}/xorg/xcompose"

# ICEAuthority
export ICEAUTHORITY="${XDG_CACHE_HOME:-$HOME/.cache}/ICEauthority"

