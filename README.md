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
We use doom emacs at the moment (though I would like to replace it with a
stripped down reimplementation following the same core idea because it's really
well designed, just too much going on).

To setup run:
```shell
# pull down doom from github
git submodule update --init --recursive

# generate envvar file
doom --localdir $XDG_CACHE_HOME/emacs env    

 # sync our config with doom
doom --localdir $XDG_CACHE_HOME/emacs sync 
```

Alternatively just leave it up to `setup-dotfiles`.

If help is needed use the doom or doom.cmd scripts.

For modifications to our config, see `$XDG_CONFIG_HOME/doom`.

NOTE: doom puts local directory inside the dotfiles repo
(`emacs/.config/emacs/.local`) instead of the actual user's home directory by
default, causing issues due to being unable to find it on a normal emacs run
post-sync, so on `doom sync`-esque commands we need to manually move them over
to the `XDG_CACHE_HOME/emacs` directory by setting the switch: `--localdir
$XDG_CONFIG_HOME/emacs` whenever running doom. Alternatively the `DOOMDIR` and
`DOOMLOCALDIR` environment variables need to be set prior to running emacs and
doom commands (this is setup in the bash config already).

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
layout wherever feasible)

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

