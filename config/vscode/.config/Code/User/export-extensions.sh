#!/usr/bin/env sh

if ! type code >/dev/null; then
  exit 1
fi

TARGET_PATH="${XDG_CONFIG_DIR:-${HOME}/.config}/Code/User/extensions.txt"
echo "Saving to ${TARGET_PATH}..."
code --list-extensions | sort | uniq >"${TARGET_PATH}"
