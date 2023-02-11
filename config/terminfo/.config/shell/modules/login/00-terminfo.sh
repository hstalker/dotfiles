#!/usr/bin/env sh

# Sometimes this isn't set by environment.d. Not clear why
if [ -n "${TERMINFO}" ]; then
  silence_output assert_directory "${TERMINFO}"
fi
