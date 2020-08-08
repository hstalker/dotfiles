#!/usr/bin/env sh

# This causes dumb issues with client connection to this box
# XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"; export XAUTHORITY
XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xinitrc"; export XINITRC
XSERVERRC="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xserverrc"; export XSERVERRC
XCOMPOSEFILE="${XDG_CONFIG_HOME:-$HOME/.config}/xorg/xcompose"
export XCOMPOSEFILE
XCOMPOSECACHE="${XDG_CACHE_HOME:-$HOME/.cache}/xorg/xcompose"
export XCOMPOSECACHE

# ICEAuthority
ICEAUTHORITY="${XDG_CACHE_HOME:-$HOME/.cache}/ICEauthority"
export ICEAUTHORITY

