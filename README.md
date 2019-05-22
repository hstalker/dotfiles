# My Dotfiles
A repo for storing my personal configuration files for software development.
*Not really intended for public consumption, but I won't stop you*
Uses GNU Stow for symlink management.

## Prerequisites
 * Git (to clone this repo somewhere local).
 * GNU Stow
 * Bash

## Support
Currently officially supports the following platforms:
 * Debian Stretch (Stable)

## Configurations
Currently includes configurations for:
 * Vim
 * Emacs
 * Clang-Format
 * Clang-Tidy
 * TMux
 * Bash
 * Git
 * Dircolors
 * Xresources

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
 * Start Vim.
 * Run `:PlugUpgrade`and then `:PlugUpdate`.
 * Run `:PlugInstall`.
 * Exit Vim

### Emacs
 * Start emacs.
 * Wait.
 * Complete any prompts.
 * Wait some more.
 * Rinse and repeat until done (or heat death of the universe).

## Usage Instructions
The following configurations support override configs for the current user:
 * vim (`~/.vimrc.local`)
 * tmux (`~/.tmux.conf.local`)
 * bash (`~/.exports.local`, `~/.functions.local`, `~/.aliases.local`,
   `~/.bashrc.local`, `~/.profile`, `~/.bash_profile.local`,
   `~/.bash_aliases`)

By adding your own versions of the files listed above, you may stack
your own customisations on top of the ones provided.

Bash customisations should preferably be placed within one of:

 * `.functions.local`
 * `.aliases.local`
 * `.exports.local`

first, and then in `.bashrc.local` if more general.

In other words, try to follow a rule of most-to-least specificity.

## Extension
### Platforms
 * Add new platform install scripts etc. to `platform/$DISTRO_packages_install`
 * Update `bash/.exports` and `setup-dotfiles` to handle identification/install
   steps for the new platforms.

### Tools/Configurations
 * Add the appopriate stow lines to `setup-dotfiles`.
 * Make a folder just to store this set of configs in the repo directory.
 * Add an appropriate .stow-local-ignore to that directory.
 * Add appropriate lines to the .gitignore in the repo directory.

## Ideas
 * Add a templating mechanism and more complex install step to pick out
   individual tools/configs to install.
 * Use a templating mechanism to customise colour schemes on a global scale.

