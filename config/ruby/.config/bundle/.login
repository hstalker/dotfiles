#!/usr/bin/env sh

silence_output assert_directory "${BUNDLE_USER_CONFIG}"
silence_output assert_directory "${BUNDLE_USER_CACHE}"
silence_output assert_directory "${BUNDLE_USER_PLUGIN}"
