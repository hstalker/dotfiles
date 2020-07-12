# This script is used by sh and ksh, and also bash if .bash_profile isn't
# present when they are login shells.
# We need to load different scripts for some different shells, so make sure
# both .profile and .bash_profile are distinct and present in the top-level.

# The path at $ENV is run when the shell is interactive.
# $ENV needs to be set in order to allow subsequent non-login interactive
# invocations of sh and ksh load startup scripts correctly.
if [ -n "$KSH_VERSION" ]; then
  . "${XDG_CONFIG_HOME:-$HOME/.config}/ksh/env"
  . "${XDG_CONFIG_HOME:-$HOME/.config}/ksh/login"
else
  . "${XDG_CONFIG_HOME:-$HOME/.config}/sh/env"
  . "${XDG_CONFIG_HOME:-$HOME/.config}/sh/login"
  # Set this to show future sourced files that we are a login shell (not a way
  # to tell this under sh normally)
  IS_LOGIN_SHELL=1
fi

