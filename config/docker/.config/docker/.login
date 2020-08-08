#!/usr/bin/env sh

# We don't actually provide docker configuration scripts, as they are likely to
# be usage specific
DOCKER_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/docker"
export DOCKER_CONFIG
MACHINE_STORAGE_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/docker-machine"
export MACHINE_STORAGE_PATH

