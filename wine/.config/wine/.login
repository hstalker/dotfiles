#!/usr/bin/env sh

[ ! -d "${XDG_DATA_HOME:-$HOME/.local/share}"/wineprefixes ] && mkdir "${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes"
export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"

