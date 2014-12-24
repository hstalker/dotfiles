# normal options
HISTFILE=~/.histfile
HISTSIZE=2500
SAVEHIST=2500
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
bindkey "^R" history-incremental-search-backward
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey -M viins 'jj' vi-cmd-mode
#export KEYTIMEOUT=1
source ~/.opp/opp.zsh
source ~/.opp/surround.zsh
source ~/.opp/textobj-between.zsh

# set prompt:
autoload -U colors && colors
PROMPT="%{$fg_no_bold[green]%}%n%{$reset_color%}@%{$fg_no_bold[magenta]%}%m%{$reset_color%}%1(j. [%j].) %{$fg_bold[white]%}%~%{$reset_color%} %# "

# set aliases:
alias c="clear"
alias e="exit"
alias pacupg='sudo pacmatic -Syu'   # Synchronize with repositories and then upgrade packages that are out of date on the local system.
alias pacdl='pacmatic -Sw'          # Download specified package(s) as .tar.xz ball
alias pacin='sudo pacmatic -S'      # Install specific package(s) from the repositories
alias pacins='sudo pacmatic -U'     # Install specific package not from the repositories but from a file 
alias pacre='sudo pacmatic -R'      # Remove the specified package(s), retaining its configuration(s) and required dependencies
alias pacrem='sudo pacmatic -Rns'   # Remove the specified package(s), its configuration(s) and unneeded dependencies
alias pacrep='pacmatic -Si'         # Display information about a given package in the repositories
alias pacreps='pacmatic -Ss'        # Search for package(s) in the repositories
alias pacloc='pacmatic -Qi'         # Display information about a given package in the local database
alias paclocs='pacmatic -Qs'        # Search for package(s) in the local database
alias paclo="pacmatic -Qdt"         # List all packages which are orphaned
alias pacc="sudo pacmatic -Scc"     # Clean cache - delete all not currently installed package files
alias paclf="pacmatic -Ql"          # List all files installed by a given package
alias pacown="pacmatic -Qo"         # Show package(s) owning the specified file(s)
alias pacexpl="pacmatic -D --asexp" # Mark one or more installed packages as explicitly installed 
alias pacimpl="pacmatic -D --asdep" # Mark one or more installed packages as non explicitly installed

# Additional pacman alias examples
alias pacupd='sudo pacmatic -Sy && sudo abs'    # Update and refresh the local package and ABS databases against repositories
alias pacinsd='sudo pacmatic -S --asdeps'       # Install given package(s) as dependencies
alias pacmir='sudo pacmatic -Syy'               # Force refresh of all package lists after updating /etc/pacman.d/mirrorlist

# Misc utility aliases
get_battery_percentage() {
    acpi -b | awk "{print $1}" | sed 's/\([^:]*\): \([^,]*\), \([0-9]*\)%.*/\3/'
}
alias batt = get_battery_percentage()

zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
