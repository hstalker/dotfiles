#!/usr/bin/env sh

PYTHONHISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/python/python_history"
export PYTHONHISTORY
# SetupTools
PYTHON_EGG_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/python-eggs"
export PYTHON_EGG_CACHE
# PyLint
PYLINTHOME="${XDG_CACHE_HOME:-$HOME/.cache}/pylint"; export PYLINTHOME

