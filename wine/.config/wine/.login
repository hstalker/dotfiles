#!/usr/bin/env sh

[ ! -d "${XDG_DATA_HOME}"/wineprefixes ] && mkdir "${XDG_DATA_HOME}/wineprefixes"
export WINEPREFIX="${XDG_DATA_HOME}/wineprefixes/default"

