#!/usr/bin/env sh

# This causes dumb issues with client connection to this box
# export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
export XINITRC="${XDG_CONFIG_HOME}/xorg/xinitrc"
export XSERVERRC="${XDG_CONFIG_HOME}/xorg/xserverrc"
export XCOMPOSEFILE="${XDG_CONFIG_HOME}/xorg/xcompose"
export XCOMPOSECACHE="${XDG_CACHE_HOME}/xorg/xcompose"

# ICEAuthority
export ICEAUTHORITY="${XDG_CACHE_HOME}/ICEauthority"

