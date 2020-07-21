#!/usr/bin/env sh

# NCurses
export TERMINFO="${XDG_DATA_HOME:-$HOME/.local/share}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME:-$HOME/.local/share}/terminfo:/usr/share/terminfo"

