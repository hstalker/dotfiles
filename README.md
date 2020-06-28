# My Dotfiles
A repo for storing my personal configuration files for software development.

*Not really intended for public consumption, but I won't stop you*

Uses GNU Stow for symlink management.

General philosophy:

 * Follow XDG layout where possible.
 * Use generally available tools where possible.
 * Make it possible to pull individual pieces out of the repo where possible.
 * Follow a pattern of main configuration files including a local configuration
   file allowing for environment specific overrides and secrets.

## Prerequisites
 * Git (to clone this repo somewhere local).
 * GNU Stow (Though technically you can manually copy/symlink).
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
 * XInit.
 * XModMap.
 * SSH.
 * GPG.
 * Ack.
 * IRuby.
 * NPM.
 * Readline.

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

Alternatively, to do setup all in batch form run:
```shell
vim +PlugUpgrade +PlugUpdate +qall
```

The Vim configuration is split into a minimal plugin-less core and a plugin
heavy setup. Minimal vim can be started by setting the environment variable
`$VIM_MINIMAL` prior to launch.

Local customisations can be placed in $XDG_CONFIG_HOME/vim/custom.vim.

### Emacs
In the process of writing a full highly customised config.

## Usage Instructions
The following configurations support override configs for the current user:
 * vim (`$XDG_CONFIG_HOME/vim/vimrc.local`)
 * tmux (`$XDG_CONFIG_HOME/tmux/tmux.conf.local`)
 * bash (`$XDG_CONFIG_HOME/bash/bashrc.local`, `~/.profile`,
   `$XDG_CONFIG_HOME/bash/bash_profile.local`, `~/.bash_aliases`)

By adding your own versions of the files listed above, you may stack
your own customisations on top of the ones provided.

Bash customisations/overrides should preferably be placed within
`$XDG_CONFIG_HOME/bash/bashrc.local` or
`$XDG_CONFIG_HOME/bash/bash_profile.local`

Generally package paths for things like emacs and vim are directed into an
appropriate `~/.cache/vim` or `~/.cache/emacs` directory in order to avoid
changes in the tree of this repo's clone wherever possible. `~/.local/`
contains a `bin` directory that is automatically added to `PATH` for temporary
or personal binaries.

Generally config paths for things are stored in ~/.config (i.e. we follow XDG
layout wherever feasible).

## Extension
### Platforms
 * Add new platform install scripts etc. to `platform/$DISTRO/`.
 * Update `$XDG_CONFIG_HOME/bash/exports` and `setup-dotfiles` to handle
   identification/install steps for the new platforms.

### Tools/Configurations
 * Add the appropriate stow lines to `setup-dotfiles`.
 * Make a folder just to store this set of configs in the repo directory.
 * Add an appropriate .stow-local-ignore to that directory.
 * Add appropriate lines to the .gitignore in the repo directory.

