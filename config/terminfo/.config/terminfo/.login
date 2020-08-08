#!/usr/bin/env sh

# NCurses
TERMINFO="${XDG_DATA_HOME:-$HOME/.local/share}/terminfo"; export TERMINFO
TERMINFO_DIRS="${XDG_DATA_HOME:-$HOME/.local/share}/terminfo:/usr/share/terminfo"
export TERMINFO_DIRS

