#!/usr/bin/env sh

# We don't actually provide nuget configuration scripts, as they are likely to
# be usage specific
NUGET_PACKAGES="${XDG_CACHE_HOME:-$HOME/.cache}/NuGetPackages"
export NUGET_PACKAGES

