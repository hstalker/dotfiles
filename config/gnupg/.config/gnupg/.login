#!/usr/bin/env sh

assign_export GNUPGHOME "${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"

