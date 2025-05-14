#!/usr/bin/env sh

assign_export FZF_COMPLETION_TRIGGER '**'
assign_export FZF_COMPLETION_OPTS '--border --info=inline'

assign_export FZF_COMPLETION_PATH_OPTS '--walker file,dir,follow,hidden'

# Assume candidates should be dirs
assign_export FZF_COMPLETION_DIR_COMMANDS 'cd pushd rmdir tree'
assign_export FZF_COMPLETION_DIR_OPTS '--walker dir,follow,hidden'

# If we have access to fd, prefer that for speed
if silence command -v fd; then
  assign_export FZF_DEFAULT_COMMAND \
    "fd -c always -t f --strip-cwd-prefix -HL -E .git -E node_modules --ignore-file .gitignore --ignore-file ${HOME:-~}/.gitignore"
  # I actually use these Emacs bindings in the shell, generally prefer manual
  # trigger expansion
  assign_export FZF_CTRL_T_COMMAND ""
  assign_export FZF_ALT_C_COMMAND ""
fi
# Assign an empty FZF_CTRL_T_COMMAND etc. before sourcing the setup script if
# you want to disable the binding
assign_export FZF_DEFAULT_OPTS '--height 50% --ansi'
assign_export FZF_CTRL_R_OPTS ''
assign_export FZF_CTRL_T_OPTS ''
assign_export FZF_ALT_C_OPTS  ''
