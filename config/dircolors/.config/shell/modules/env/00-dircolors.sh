#!/usr/bin/env sh

# Setup directory colors
load_dircolors_config() {
  if [ -x /usr/bin/dircolors ]; then
    if [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/dircolors/dircolors" ]; then
      eval \
        "$(dircolors -b "${XDG_CONFIG_HOME:-$HOME/.config}/dircolors/dircolors")"
    else
      eval "$(dircolors -b)"
    fi
  fi

  return 0
}

# Enable colour support of things like `ls`
load_dircolors_config
