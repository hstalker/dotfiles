###############################################################################
# .zshrc
###############################################################################

source ~/shell-settings.sh

# normal options
HISTFILE=~/.histfile
HISTSIZE=2500
SAVEHIST=2500

setopt nomatch correctall
unsetopt appendhistory autocd beep extendedglob notify

# set prompt
autoload -U colors && colors
PS1="%{$fg_bold[brown]%}[ %{$reset_color%}%{$fg_bold[blue]%}%n@%m %{$reset_color%}%{$fg_bold[brown]%}] %{$reset_color%}%{$fg_bold[blue]%}%~
%{$reset_color%}%{$fg_bold[lblue]%}\$%{$reset_color%}%{$fg_normal[brown]%}> %{$reset_color%}"

zstyle :compinstall filename '~/.zshrc'

autoload -U compinit
compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
