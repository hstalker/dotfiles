#!/usr/bin/env sh

silence_output assert_directory "$(dirname "${CCACHE_CONFIGPATH}")"
silence_output assert_directory "${CCACHE_DIR}"
