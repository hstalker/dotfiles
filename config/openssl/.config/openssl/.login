#!/usr/bin/env sh

# Just make sure we put the rand file in the cache
assign_export RANDFILE "${XDG_CACHE_HOME:-$HOME/.cache}/openssl/rnd"

