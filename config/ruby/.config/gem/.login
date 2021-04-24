#!/usr/bin/env sh

assign_export GEM_HOME "${XDG_DATA_HOME:-$HOME/.local/share}/gem"
assign_export GEM_SPEC_CACHE "${XDG_CACHE_HOME:-$HOME/.cache}/gem"

