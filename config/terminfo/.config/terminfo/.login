#!/usr/bin/env sh

# NCurses
assign_export TERMINFO "${XDG_DATA_HOME:-$HOME/.local/share}/terminfo"
assign_export TERMINFO_DIRS \
  "${XDG_DATA_HOME:-$HOME/.local/share}/terminfo:/usr/share/terminfo"

