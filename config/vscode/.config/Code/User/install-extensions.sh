#!/usr/bin/env sh

if ! type code >/dev/null; then
  exit 1
fi

SRC_PATH="${XDG_CONFIG_HOME:-${HOME}/.config}/Code/User/extensions.txt"
echo "Loading ${SRC_PATH}..."
cat "${SRC_PATH}" | xargs -n1 code --force --install-extension
