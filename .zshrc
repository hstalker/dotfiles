###############################################################################
# .zshrc
###############################################################################

source ~/shellsettings.sh

# normal options
HISTFILE=~/.histfile
HISTSIZE=2500
SAVEHIST=2500

setopt nomatch correctall
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

# Set prompt
autoload -U colors && colors
PS1="%{$fg_bold[brown]%}[ %{$reset_color%}%{$fg_bold[blue]%}%n@%m %{$reset_color%}%{$fg_bold[brown]%}] %{$reset_color%}%{$fg_bold[blue]%}%~
%{$reset_color%}%{$fg_bold[lblue]%}\$%{$reset_color%}%{$fg_normal[brown]%}> %{$reset_color%}"

zstyle :compinstall filename '~/.zshrc'

autoload -U compinit
compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
