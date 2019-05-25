#! /usr/bin/env bash

source ~/.exports
source ~/.functions
source ~/.aliases

# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend
# for setting history length
HISTSIZE=10000
HISTFILESIZE=10000
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# make less more friendly for non-text input files
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# enable colour support of ls
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# enable programmable completion features (you don't need to enable
# this if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

colour1="\[$(tput setaf 1)\]"
colour4="\[$(tput setaf 4)\]"
colour6="\[$(tput setaf 6)\]"
colourreset="\[\033[0m\]"
PS1="$colour4[\$(basename \${PWD})/]" # path
PS1+="$colour1\$(get-git-branch)$colour6$ " # git branch
PS1+="$colourreset" # colour back to normal
unset colour1 colour4 colour6 colourreset

[[ -x /usr/bin/ssh-agent ]] && start-ssh-agent &>/dev/null

# put anything you don't want to do every shell invocation here
case "$-" in
  *i*) ;; # interactive
  *) ;; # non-interactive
esac

[[ -f ~/.bashrc.local ]] && source ~/.bashrc.local

