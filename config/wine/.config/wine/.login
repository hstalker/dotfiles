#!/usr/bin/env sh

silence_output \
  assert_directory "${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes"
assign_export WINEPREFIX \
  "${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"

