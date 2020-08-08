#!/usr/bin/env bash
# Run when shell is non-interactive via $BASH_ENV, or when shell is non-login
# and interactive

# Load env when we are a non-login interactive shell or a remote shell, but not
# in an interactive login shell as that would've already loaded it
if ! shopt -q login_shell; then
  . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/env"
fi

# Run interactive only when in an interactive shell
case $- in
  *i*) . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/interactive";;
esac

