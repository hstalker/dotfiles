#!/usr/bin/env zsh

if silence command -v direnv; then
  eval "$(direnv hook zsh)"
fi
