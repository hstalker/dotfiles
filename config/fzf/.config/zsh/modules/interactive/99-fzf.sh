#!/usr/bin/env zsh

if silence command -v fzf; then
  eval "$(fzf --zsh)"
fi
