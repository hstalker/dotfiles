#!/usr/bin/env sh

silence_output assert_directory "$(dirname "${GTK_RC_FILES}")"
silence_output assert_directory "$(dirname "${GTK2_RC_FILES}")"
