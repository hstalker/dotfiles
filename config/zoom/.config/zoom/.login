#!/usr/bin/env sh

# Zoom manages its configuration files itself, and mixes it with data because
# of course it does.
assign_export SSB_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zoom"

