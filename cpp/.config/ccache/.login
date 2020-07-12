#!/usr/bin/env sh

# We don't actually give a ccache configuration here, as it's likely to be
# system specific
export CCACHE_CONFIGPATH="${XDG_CONFIG_HOME}/ccache/config"
export CCACHE_DIR="${XDG_CACHE_HOME}/ccache"

