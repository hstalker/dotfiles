#!/usr/bin/env bash

if silence command -v direnv; then
  eval "$(direnv hook bash)"
fi
