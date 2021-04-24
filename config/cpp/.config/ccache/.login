#!/usr/bin/env sh

# We don't actually give a ccache configuration here, as it's likely to be
# system specific
assign_export CCACHE_CONFIGPATH \
  "${XDG_CONFIG_HOME:-$HOME/.config}/ccache/config"
assign_export CCACHE_DIR \
  "${XDG_CACHE_HOME:-$HOME/.cache}/ccache"

