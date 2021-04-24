#!/usr/bin/env sh

assign_export PYTHONHISTORY \
  "${XDG_CACHE_HOME:-$HOME/.cache}/python/python_history"
# SetupTools
assign_export PYTHON_EGG_CACHE "${XDG_CACHE_HOME:-$HOME/.cache}/python-eggs"
# PyLint
assign_export PYLINTHOME "${XDG_CACHE_HOME:-$HOME/.cache}/pylint"

