#!/usr/bin/env bash

if silence command -v fzf; then
  eval "$(fzf --bash)"
fi
