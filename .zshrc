###############################################################################
# .zshrc
###############################################################################

source ~/shellsettings.sh

# normal options
HISTFILE=~/.histfile
HISTSIZE=2500
SAVEHIST=2500

setopt nomatch
unsetopt appendhistory autocd beep extendedglob notify

# vim style configuration
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey "^R" history-incremental-search-backward
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey -M viins 'kj' vi-cmd-mode
source ~/.opp/opp.zsh
source ~/.opp/surround.zsh
source ~/.opp/textobj-between.zsh

# Set prompts
autoload -U colors && colors
PROMPT="%{$fg_no_bold[green]%}%n%{$reset_color%}@%{$fg_no_bold[magenta]%}%m%{$reset_color%}%1(j. [%j].) %{$fg_bold[white]%}%~%{$reset_color%} %# "

zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
