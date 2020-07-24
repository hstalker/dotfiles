#!/usr/bin/env sh

if [ ! -d "${XDG_DATA_HOME:-$HOME/.local/share}"/wineprefixes ]; then
  mkdir "${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes"
fi
WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export WINEPREFIX

