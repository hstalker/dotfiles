#!/usr/bin/env sh

assign_export IPYTHONDIR \
  "${XDG_CONFIG_HOME:-$HOME/.config}/jupyter"
assign_export JUPYTER_CONFIG_DIR \
  "${XDG_CONFIG_HOME:-$HOME/.config}/jupyter"

