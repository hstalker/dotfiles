#!/usr/bin/env sh

silence_output assert_directory "${XDG_STATE_HOME:-$HOME/.local/state}/less"
