#!/usr/bin/env sh

# Just make sure we put the rand file in the cache
RANDFILE="${XDG_CACHE_HOME:-$HOME/.cache}/openssl/rnd"; export RANDFILE

