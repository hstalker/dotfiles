#!/usr/bin/env bash
# Ensure env is run first by all login shells
. "${XDG_CONFIG_HOME:-$HOME/.config}/bash/env"

# Run login script
# This check is technically redundant as .bash_profile is only read by login
# shells.
if shopt -q login_shell; then
  . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/login"
fi

# Run interactive if we are an interactive shell
case $- in
  *i*) . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/interactive";;
esac

