#!/usr/bin/env sh

assign_export VAGRANT_HOME "${XDG_DATA_HOME:-$HOME/.local/share}/vagrant"
assign_export VAGRANT_ALIAS_FILE \
  "${XDG_DATA_HOME:-$HOME/.local/share}/vagrant/aliases"

