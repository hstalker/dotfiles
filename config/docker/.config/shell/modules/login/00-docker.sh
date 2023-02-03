#!/usr/bin/env sh

# We don't actually provide docker configuration scripts, as they are likely to
# be usage specific
silence_output assert_directory "${DOCKER_CONFIG}"
