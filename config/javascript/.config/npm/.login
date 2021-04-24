#!/usr/bin/env sh

assign_export NPM_CONFIG_USERCONFIG \
  "${XDG_CONFIG_HOME:-$HOME/.config}/npm/npmrc"

# Node
assign_export NODE_REPL_HISTORY \
  "${XDG_DATA_HOME:-$HOME/.local/share}/node_repl_history"

