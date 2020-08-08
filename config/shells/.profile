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
  # Set exit trap so that we can simulate a ~/.logout script
  trap '. "${XDG_CONFIG_HOME:-$HOME/.config}/ksh/logout"' EXIT
elif [ -n "$BASH" ]; then
  # We might be in an unexpected shell due to graphical login hacks (think gdm
  # etc.) sourcing ~/.profile directly.
  # If so, just forward to those respective profiles
  . "$HOME/.bash_profile"
elif [ -n "$ZSH_NAME" ]; then
  # Ditto above
  . "$HOME/.zshenv"
  . "$HOME/.zprofile"
else
  . "${XDG_CONFIG_HOME:-$HOME/.config}/sh/env"
  . "${XDG_CONFIG_HOME:-$HOME/.config}/sh/login"
  trap '. "${XDG_CONFIG_HOME:-$HOME/.config}/sh/logout"' EXIT
  # Set this to show future sourced files that we are a login shell (not a way
  # to tell this under sh normally)
  IS_LOGIN_SHELL=1
fi

