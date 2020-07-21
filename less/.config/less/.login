#!/usr/bin/env sh

mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/less"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/less/history"
export LESSKEY="${XDG_CONFIG_HOME:-$HOME/.config}/less/lesskey"

