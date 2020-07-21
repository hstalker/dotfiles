#!/usr/bin/env sh

# We don't actually provide docker configuration scripts, as they are likely to
# be usage specific
export DOCKER_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/docker"
export MACHINE_STORAGE_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/docker-machine"

