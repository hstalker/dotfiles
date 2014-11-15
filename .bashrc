#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=vim

alias ls='ls --color=auto'
export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\]@\[\e[0m\]\[\e[01;31m\]\h\[\e[0m\]\[\e[00;37m\]-\[\e[0m\]\[\e[00;32m\][\W]\[\e[0m\]\[\e[00;37m\]> \[\e[0m\]"
