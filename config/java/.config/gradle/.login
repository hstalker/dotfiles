#!/usr/bin/env sh

assign_export GRADLE_USER_HOME \
  "${XDG_DATA_HOME:-$HOME/.local/share}/gradle"

