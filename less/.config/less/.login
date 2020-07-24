#!/usr/bin/env sh

mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/less"
LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/less/history"
export LESSHISTFILE
LESSKEY="${XDG_CONFIG_HOME:-$HOME/.config}/less/lesskey"
export LESSKEY

