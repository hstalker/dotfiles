# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# set editor
export ALTERNATE_EDITOR="nano"
export EDITOR='vim'
export VISUAL='vim'

# aliases to quickly do git certificate trust issue workaround
# (avoid using these)
alias git-disable-ssl='export GIT_SSL_NO_VERIFY=1'
alias git-enable-ssl='export GIT_SSL_NO_VERIFY='

# alias to remove all tracked folders and directories
# from a local git repo's directory
alias git-clean-tracked-files='git ls-files -z | xargs -0 rm -f'
alias git-clean-tracked-dir='git ls-tree --name-only -d -r -z HEAD| sort -rz \
    | xargs -0 rmdir'
alias git-clean-tracked='git-clean-tracked-files;git-clean-tracked-dir'

# alias for generating a space separated list of packages installed via pacman
# only created on systems with pacman reachable via PATH
command -v pacman >/dev/null 2>&1 && \
    { \
        alias pacman-list-pkgs="pacman -Qqe | tr '\n' ' '"; \
    }

function setup-solarized-colours {
    # Change terminal colours as dark solarized
    echo -ne '\e]P0073642'
    echo -ne '\e]P1dc322f'
    echo -ne '\e]P1dc322f'
    echo -ne '\e]P2859900'
    echo -ne '\e]P3b58900'
    echo -ne '\e]P4268bd2'
    echo -ne '\e]P5d33682'
    echo -ne '\e]P62aa198'
    echo -ne '\e]P7eee8d5'
    echo -ne '\e]P8002b36'
    echo -ne '\e]P9cb4b16'
    echo -ne '\e]PA586e75'
    echo -ne '\e]PB657b83'
    echo -ne '\e]PC839496'
    echo -ne '\e]PD6c71c4'
    echo -ne '\e]PE93a1a1'
    echo -ne '\e]PFfdf6e3'
    clear
}
