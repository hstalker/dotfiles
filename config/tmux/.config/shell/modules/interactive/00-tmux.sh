#!/usr/bin/env sh

# >=3.1 supports ~/.config/tmux by default
# >=3.2 support $XDG_CONFIG_HOME/tmux by default
alias tmux='tmux -2 -f "${XDG_CONFIG_HOME:-${HOME}/.config}/tmux/tmux.conf"'
