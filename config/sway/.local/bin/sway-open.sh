#!/usr/bin/env sh
set -eu

if ! command -v xdg-open >/dev/null 2>&1 || \
   ! command -v xdg-mime >/dev/null 2>&1; then
  echo "xdg-open or xdg-mime not found! Please install them on your system."
  exit 1
fi

discovered_file_type="$(xdg-mime query filetype $1)"
echo "Opening file $1 of type " ${discovered_file_type} " with " \
  "$(xdg-mime query default $discovered_file_type)"
nohup xdg-open "$1" >/dev/null 2>&1 &

exit 0
