#!/usr/bin/env sh

assign_export _JAVA_OPTIONS \
  "-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java"

