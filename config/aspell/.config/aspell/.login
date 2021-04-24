#!/usr/bin/env sh

# Personal is your personal dictionary, and repl is your replacement table
# Here dictionary = list of "$word"
# Replacement table = list of "$mispelling $correct"
assign_export ASPELL_CONF \
  "per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; \
personal $XDG_CONFIG_HOME/aspell/en.pws; \
repl $XDG_CONFIG_HOME/aspell/en.prepl"

