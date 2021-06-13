#!/usr/bin/env sh

silence_output assert_directory "${PYTHONHISTORY}"

# SetupTools
silence_output assert_directory "${PYTHON_EGG_CACHE}"

# PyLint
silence_output assert_directory "${PYLINTHOME}"
