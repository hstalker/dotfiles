#!/usr/bin/env sh

silence_output \
  assert_directory "${XDG_CONFIG_HOME:-${HOME}/.config}/yarn"
silence_output \
  assert_directory "${XDG_CACHE_HOME:-${HOME}/.cache}/yarn"
silence_output \
  assert_directory "${XDG_DATA_HOME:-${HOME}/.local/share}/yarn"

