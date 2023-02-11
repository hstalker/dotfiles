#!/usr/bin/env sh

if [ -n "${JAVA_HOME}" ]; then
  silence_output assert_directory "${JAVA_HOME}"
fi
