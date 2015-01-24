###############################################################################
# General settings in an sh module for various compatible shells to source
###############################################################################
# Env. variables
export EDITOR="vim"
export VISUAL="vim"
export PATH="$HOME/.ghc/.cabal-sandbox/bin:$PATH"

# Set prompt
PS1='\[\033[01m\][ \[\033[01;34m\]\u@\h \[\033[00m\]\[\033[01m\]] \[\033[01;32m\]\w\[\033[00m\]\n\[\033[01;34m\]$\[\033[00m\]> '

# Set aliases
alias c='clear'
alias e='exit'
alias g='git'
alias ls='ls --color=auto'
alias pacupg='sudo pacmatic -Syu'   # Synchronize with repositories and then upgrade packages that are out of date on the local system.
alias pacdl='pacmatic -Sw'          # Download specified package(s) as .tar.xz ball
alias pacin='sudo pacmatic -S'      # Install specific package(s) from the repositories
alias pacins='sudo pacmatic -U'     # Install specific package not from the repositories but from a file
alias pacre='sudo pacmatic -R'      # Remove the specified package(s), retaining its configuration(s) and required dependencies
alias pacrem='sudo pacmatic -Rns'   # Remove the specified package(s), its configuration(s) and unneeded dependencies
# Remove orphans and configs of orphans
alias pacro="/usr/bin/pacman -Qtdq > /dev/null && sudo /usr/bin/pacman -Rns \$(/usr/bin/pacman -Qtdq | sed -e ':a;N;\$!ba;s/\n/ /g')"
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
alias pacupd='sudo pacmatic -Sy && sudo abs'    # Update and refresh the local package and ABS databases against repositories
alias pacinsd='sudo pacmatic -S --asdeps'       # Install given package(s) as dependencies
alias pacmir='sudo pacmatic -Syy'               # Force refresh of all package lists after updating /etc/pacman.d/mirrorlist

# Misc utility aliases
alias battery="acpi -b | awk '{print $1}' | sed 's/\([^:]*\): \([^,]*\), \([0-9]*\)%.*/\3/'"

# Start X if available
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
