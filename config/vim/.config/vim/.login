#!/usr/bin/env sh

assign_export VIMINIT \
  'let $MYVIMRC="$XDG_CONFIG_HOME/vim/core.vim" | source $MYVIMRC'

