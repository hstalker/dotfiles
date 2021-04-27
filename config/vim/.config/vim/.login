#!/usr/bin/env sh

# Tell Vim to use XDG directories for its initialization
assign_export VIMINIT \
  'let $MYVIMRC="$XDG_CONFIG_HOME/vim/core.vim" | source $MYVIMRC'

# Set Vim as the default editor
assign_export EDITOR 'vim'
# $EDITOR-style variables don't allow for passing flags. If you want that you
# need to use a wrapper script which things like (sudo)edit can directly
# execute.
assign_export VISUAL "vim-minimal"
