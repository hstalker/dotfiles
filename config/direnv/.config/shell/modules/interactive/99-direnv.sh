#!/usr/bin/env bash

if command -v direnv >/dev/null 2>&1; then
  # Only supports a few shell as of now
  if [ -n "$BASH" ]; then
    eval "$(direnv hook bash)"
  elif [ -n "$ZSH_NAME" ]; then
    eval "$(direnv hook zsh)"
  fi
fi
