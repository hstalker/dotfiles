#!/usr/bin/env sh

silence_output assert_directory "${XDG_CACHE_HOME:-$HOME/.cache}/less"
assign_export LESSHISTFILE \
  "${XDG_CACHE_HOME:-$HOME/.cache}/less/history"
assign_export LESSKEY \
  "${XDG_CONFIG_HOME:-$HOME/.config}/less/lesskey"

