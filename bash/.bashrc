#! /usr/bin/env bash

###############################################################################
# Useful functions
# add something to path without duplicating
# pass path wanted as argument
path-update() {
  [[ $# -ne 1 ]] && return 1
  case ":${PATH:=$1}:" in
    *:$1:*) ;;
    *) PATH="$1:$PATH" ;;
  esac
}

# git branch display mechanism for PS1
get-git-branch() {
  local BRANCH=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD 2>/dev/null)
  if [[ ! -z "${BRANCH}" ]]; then
    echo "(${BRANCH})"
  else
    echo "${BRANCH}"
  fi
}


###############################################################################
# Core bash configuration
# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize
# Append to the history file, don't overwrite it
shopt -s histappend


###############################################################################
# Variables and exports
path-update $HOME/.local/bin

# Colours and PS1 setup
colour1="\[$(tput setaf 1)\]"
colour4="\[$(tput setaf 4)\]"
colour6="\[$(tput setaf 6)\]"
colourreset="\[\033[0m\]"
PS1="$colour4[\$(basename \${PWD})/]" # path
PS1+="$colour1\$(get-git-branch)$colour6$ " # git branch
PS1+="$colourreset" # colour back to normal

unset colour1 colour4 colour6 colourreset

# Don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth:erasedups
# For setting history length
HISTSIZE=10000
HISTFILESIZE=10000

export ALTERNATE_EDITOR='nano'
export EDITOR='VIM_MINIMAL=1 vim'
export VISUAL='VIM_MINIMAL=1 vim'
export PAGER='less'

# Lots of applications don't respect XDG conventions
# so here are a bunch of hacks to get them working
# Set all the XDG path vars
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
# Shouldn't set this as systemd sets it up
#export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/run/user/$UID}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/local/share:/usr/share}
export XDG_CONFIG_DIRS=${XDG_CONFIG_DIRS:-/etc/xdg}

# Core system tooling
# This causes dumb issues with client connection to this box
# export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
export XINITRC="${XDG_CONFIG_HOME}/X11/xinitrc"
export XSERVERRC="${XDG_CONFIG_HOME}/X11/xserverrc"
# XCompose
export XCOMPOSEFILE="${XDG_CONFIG_HOME}/X11/xcompose"
export XCOMPOSECACHE="${XDG_CACHE_HOME}/X11/xcompose"
# Wget
export WGETRC="${XDG_CONFIG_HOME}/wget/wgetrc"
alias wget="wget --hsts-file=\"${XDG_CACHE_HOME}/wget/hsts\""
# GnuPG
export GNUPGHOME="${XDG_CONFIG_HOME}/gnupg"
# ICEAuthority
export ICEAUTHORITY="${XDG_CACHE_HOME}/ICEauthority"
# Less pager
mkdir -p "${XDG_CACHE_HOME}/less"
export LESSHISTFILE="${XDG_CACHE_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/lesskey"
# Most pager
export MOST_INITFILE="${XDG_CONFIG_HOME}/mostrc"
# NCurses
export TERMINFO="${XDG_DATA_HOME}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME}/terminfo:/usr/share/terminfo"
# OpenSSL
export RANDFILE="${XDG_CACHE_HOME}/openssl/rnd"
# Readline
export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"
# GNU Screen
export SCREENRC="${XDG_CONFIG_HOME}/screen/screenrc"
# URXVTD (rxvt daemon)
export RXVT_SOCKET="${XDG_RUNTIME_DIR}/urxvtd"
# SVN
export SUBVERSION_HOME="${XDG_CONFIG_HOME}/subversion"
# Vim
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/core.vim" | source $MYVIMRC'
# Bash
export HISTFILE="${XDG_DATA_HOME}/bash/history"
export BASH_COMPLETION_USER_FILE="${XDG_CONFIG_HOME}/bash-completion/bash_completion"
[ -r "${XDG_CONFIG_HOME}/bash-completion/bash_completion" ] && \
  . "${XDG_CONFIG_HOME}/bash-completion/bash_completion"
# Ack
export ACKRC="${XDG_CACHE_HOME}/ack/ackrc"
# ASpell
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; \
  personal $XDG_CONFIG_HOME/aspell/en.pws; \
  repl $XDG_CONFIG_HOME/aspell/en.prepl"
# Pass
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# Programming language tooling
#Python
export PYTHONHISTORY="${XDG_CACHE_HOME}/python/python_history"
# SetupTools
export PYTHON_EGG_CACHE="${XDG_CACHE_HOME}/python-eggs"
# PyLint
export PYLINTHOME="${XDG_CACHE_HOME}/pylint"
# Jupyter
export IPYTHONDIR="${XDG_CONFIG_HOME}/jupyter"
export JUPYTER_CONFIG_DIR="${XDG_CONFIG_HOME}/jupyter"

# Ruby
# IRuby
export IRBRC="${XDG_CONFIG_HOME}/irb/irbrc"
# Ruby Bundle
export BUNDLE_USER_CONFIG="${XDG_CONFIG_HOME}/bundle"
export BUNDLE_USER_CACHE="${XDG_CACHE_HOME}/bundle"
export BUNDLE_USER_PLUGIN="${XDG_DATA_HOME}/bundle"
# Ruby gems
export GEM_HOME="${XDG_DATA_HOME}/gem"
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}/gem"

# Rust
# Cargo
export CARGO_HOME="${XDG_DATA_HOME}/cargo"

# Mathematica
export MATHEMATICA_USERBASE="${XDG_CONFIG_HOME}/mathematica"

# Go
export GOPATH="${XDG_DATA_HOME}/go"

# Javascript
# Node
export NODE_REPL_HISTORY="${XDG_DATA_HOME}/node_repl_history"
# NPM
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"

# Racket (PLT Scheme)
export PLTUSERHOME="${XDG_DATA_HOME}/racket"

# Haskell
# Stack
export STACK_ROOT="${XDG_DATA_HOME}/stack"

# Java
# Gradle
export GRADLE_USER_HOME="${XDG_DATA_HOME}/gradle"

# C/C++
# CCache
export CCACHE_CONFIGPATH="${XDG_CONFIG_HOME}/ccache.config"
export CCACHE_DIR="${XDG_CACHE_HOME}/ccache"

# CUDA
export CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv"

# .NET
# NuGet
export NUGET_PACKAGES="${XDG_CACHE_HOME}/NuGetPackages"

# Virtualization
# Docker
export DOCKER_CONFIG="${XDG_CONFIG_HOME}/docker"
export MACHINE_STORAGE_PATH="${XDG_DATA_HOME}/docker-machine"
# Vagrant
export VAGRANT_HOME="${XDG_DATA_HOME}/vagrant"
export VAGRANT_ALIAS_FILE="${XDG_DATA_HOME}/vagrant/aliases"

# Cloud
# Azure Python CLI
export AZURE_CONFIG_DIR="${XDG_DATA_HOME}/azure"
# AWS CLI
export AWS_SHARED_CREDENTIALS_FILE="${XDG_CONFIG_HOME}/aws/credentials"
export AWS_CONFIG_FILE="${XDG_CONFIG_HOME}/aws/config"

# Databases
# MySQL
export MYSQL_HISTFILE="${XDG_DATA_HOME}/mysql_history"
# Postgres
[ ! -d "${XDG_CONFIG_HOME}"/pg ] && mkdir "${XDG_CONFIG_HOME}/pg"
[ ! -d "${XDG_CACHE_HOME}"/pg ] && mkdir "${XDG_CACHE_HOME}/pg"
export PSQLRC="${XDG_CONFIG_HOME}/pg/psqlrc"
export PSQL_HISTORY="${XDG_CACHE_HOME}/pg/psql_history"
export PGPASSFILE="${XDG_CONFIG_HOME}/pg/pgpass"
export PGSERVICEFILE="${XDG_CONFIG_HOME}/pg/pg_service.conf" 

# Desktop Environments
# GTK
export GTK_RC_FILES="${XDG_CONFIG_HOME}/gtk-1.0/gtkrc"
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
# KDE
export KDEHOME="${XDG_CONFIG_HOME}/kde"

# Misc applications
# MPlayer
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
# Mednafen PS1 emulator
export MEDNAFEN_HOME="${XDG_CONFIG_HOME}/mednafen"
# Atom text editor
export ATOM_HOME="${XDG_DATA_HOME}/atom"
# WINE
[ ! -d "${XDG_DATA_HOME}"/wineprefixes ] && mkdir "${XDG_DATA_HOME}/wineprefixes"
export WINEPREFIX="${XDG_DATA_HOME}/wineprefixes/default"


###############################################################################
# Aliases
if [ -x /usr/bin/dircolors ]; then
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias l.='ls -d .*'

# because I'm clumsy and accidentally delete stuff
alias rm='rm -iI --preserve-root'
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

alias h='history'
alias j='jobs'

alias vim-minimal='VIM_MINIMAL=1 vim'
alias vi='vim'
alias tmux='tmux -2 -f "$XDG_CONFIG_HOME"/tmux/tmux.conf'

# aliases to quickly do git certificate trust issue workaround
alias git-ssl-off='GIT_SSL_NO_VERIFY=1 git'
alias git-ssl-on='GIT_SSL_NO_VERIFY=0 git'

# alias to remove all tracked folders and directories
# from a local git repo's directory
alias git-clean-tracked-files='git ls-files -z | xargs -0 rm'
alias git-clean-tracked-dir='git ls-tree --name-only -d -r -z HEAD \
  | sort -rz \
  | xargs -0 rmdir'
alias git-clean-tracked='git-clean-tracked-files; git-clean-tracked-dir'

# various hardware info
alias cpu-count='nproc --all'
alias meminfo='free -m -l -t'
alias cpuinfo='lscpu'
alias diskinfo='df -H'


###############################################################################
# Miscellaneous other configuration
# Make less more friendly for non-text input files
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# Enable colour support of ls
if [ -x /usr/bin/dircolors ]; then
  test -r ${XDG_CONFIG_HOME}/dircolors/dircolors && \
    eval "$(dircolors -b ${XDG_CONFIG_HOME}/dircolors/dircolors)" || \
    eval "$(dircolors -b)"
fi

# Enable programmable completion features (you don't need to enable
# this if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Put anything you don't want to do every shell invocation here
case "$-" in
  *i*) ;; # interactive
  *) ;; # non-interactive
esac

# Load local install overrides
[[ -f ${XDG_CONFIG_HOME}/bash/custom.bash ]] && \
  . ${XDG_CONFIG_HOME}/bash/custom.bash
