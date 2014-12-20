# normal options
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
PS1="[%T] %B%m%b:%c%# "
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
setopt nomatch
unsetopt appendhistory autocd beep extendedglob notify

# vim style configuration
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey -M viins 'jj' vi-cmd-mode
#export KEYTIMEOUT=1
source ~/.opp/opp.zsh
source ~/.opp/surround.zsh
source ~/.opp/textobj-between.zsh

zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit

