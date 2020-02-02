# My Dotfiles
A repo for storing my personal configuration files for software development.

*Not really intended for public consumption, but I won't stop you*

Uses GNU Stow for symlink management.

## Prerequisites
 * Git (to clone this repo somewhere local).
 * GNU Stow.
 * Bash.

## Support
Currently officially supports the following platforms:
 * Debian Stretch (Stable w/ backports).
 * Debian Buster (Stable w/ backports).
 * Basically any recent Debian (probably - untested).

## Configurations
Currently includes configurations for:
 * Vim.
 * Emacs.
 * Tmux.
 * Bash.
 * Git.
 * Dircolors.
 * Xresources.
 * SSH.
 * GPG.

## Install Instructions
```shell
cd directory-of-this-repo
./setup-dotfiles
```
Follow the on screen prompts.

### Shell
Either:
 * Start a new shell.
or
 * Reboot/logout & login.

### Vim
Should configure on its own when you run the setup script, but manual steps:

 * Start Vim.
 * Run `:PlugUpgrade`and then `:PlugUpdate`.
 * Run `:PlugInstall`.
 * Exit Vim

### Emacs
Should configure on its own when you run the setup script, but manual steps:

 * Start emacs.
 * Wait.
 * Complete any prompts.
 * Wait some more.
 * Rinse and repeat until done (or heat death of the universe).

## Usage Instructions
The following configurations support override configs for the current user:
 * vim (`$XDG_CONFIG_HOME/vim/vimrc.local`)
 * tmux (`$XDG_CONFIG_HOME/tmux/tmux.conf.local`)
 * bash (`$XDG_CONFIG_HOME/bash/exports.local`, `$XDG_CONFIG_HOME/bash/functions.local`,
   `$XDG_CONFIG_HOME/bash/aliases.local`, `$XDG_CONFIG_HOME/bash/bashrc.local`, `~/.profile`,
   `$XDG_CONFIG_HOME/bash/bash_profile.local`, `~/.bash_aliases`)

By adding your own versions of the files listed above, you may stack
your own customisations on top of the ones provided.

Bash customisations/overrides should preferably be placed within one of:

 * `$XDG_CONFIG_HOME/bash/functions.local`
 * `$XDG_CONFIG_HOME/bash/aliases.local`
 * `$XDG_CONFIG_HOME/bash/exports.local`

first, and then in `$XDG_CONFIG_HOME/bash/bashrc.local` if more general.

In other words, try to follow a rule of most-to-least specificity.

Generally package paths for things like emacs and vim are directed into an
appropriate `~/.cache/vim` or `~/.cache/emacs` directory in order to avoid
changes in the tree of this repo's clone wherever possible. `~/.local/`
contains a `bin` directory that is automatically added to `PATH` for temporary
or personal binaries.

Generally config paths for things are stored in ~/.config (i.e. we follow XDG
layout wherever feasible)

## Extension
### Platforms
 * Add new platform install scripts etc. to `platform/$DISTRO/`.
 * Update `$XDG_CONFIG_HOME/bash/exports` and `setup-dotfiles` to handle identification/install
   steps for the new platforms.

### Tools/Configurations
 * Add the appropriate stow lines to `setup-dotfiles`.
 * Make a folder just to store this set of configs in the repo directory.
 * Add an appropriate .stow-local-ignore to that directory.
 * Add appropriate lines to the .gitignore in the repo directory.

## Ideas
 * Add a templating mechanism and more complex install step to pick out
   individual tools/configs to install. Seems like a good use-case for ansible,
   but if you're going to go for ansible you may as well abstract out all the
   actual configs into a bunch of simpler variables and templates. Frankly ansible
   seems like overkill here; the scope of the project is not to provision machines,
   but to link some basic configuration files.
 * Use a templating mechanism to customise colour schemes on a global scale.
 * Change so that (as much as possible) the configs placed in name specific
   subdirectories with nearly empty configs in the top-level sourcing them.
   This would allow us to better break up the configurations for things like vim
   into multiple files without cluttering the user's `$HOME` directory.
 * Add some useful scripts and utilities to `~/.local/bin` (e.g. cpu stats etc.).
