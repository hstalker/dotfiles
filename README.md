# My Dotfiles
A repo for storing my personal configuration files for software development
under Linux.

*Not really intended for public consumption*

Unfortunately I require the ability to stow or symlink configuration files on
various Linux distros, so my dotfiles themselves are kept in a form making that
easy. Current plans are to move to a NixOS configuration, and use these
dotfiles as-is via home-manager. If the requirement of supporting other distros
could be dropped, it is likely that this could be made considerably simpler,
alas the world isn't so simple. Hopefully in the not so distant future I can
have a single source of truth for the vast, vast majority of configuration for
all my systems in this repo.

Future Goals:
* Add emacs configuration
* Add bspwm/rofi/polybar configuration
* Move to NixOS, whilst maintaining near full stowability of dotfiles to
  non-Nix systems.

General philosophy:

* Follow XDG layout where possible.
* Use generally available tools where possible.
* Make it possible to pull individual pieces out of the repo where possible.
* Make it highly customizable for local installations (i.e. override points,
  globally consistent theme selection, package pinning/disabling, package
  selection)

## Prerequisites
 * Git (to clone this repo somewhere local).
 * GNU Make (For automating running stow on the packages).
 * GNU Stow (Though technically you can manually copy/symlink).

## Configurations
Currently includes some configuration for the following pieces of software:
*NOTE:* Some of the following "packages" may not contain anything but shell
scripts for use by the `shells` package in order to set aliases and environment
variables.

* Editors
    * Vim - Lightweight simple text editing experience
    * Emacs - Preferred highly customized IDE experience
* Shells (These are merged into a single "package" with shared configuration)
    * Sh - Common everywhere (Unix-likes)
    * Bash - Common on Linux
    * KSh - Common on BSDs and old Unices
    * ZSh - Preferred daily driver
* Terminals
    * URxvt - Lightweight, but lacks good support for a lot of features
    * XTerm - Ubiquitous, but also lacking in some features
* Tools
    * Ack - Classic search tool
    * Aspell - Spelling aid
    * Docker - Containerization
    * Vagrant - Virtualization
    * GnuPG - Signing and encryption tools
    * Less - Pager that's better than the basic `more` pager
    * MPV - Media player that knocks the socks off of the competition
    * Terminfo - Terminal information querying tools
    * WGet - Fetch files over the internet
    * WINE - Windows emulation layer
    * Xorg utilities - Sets up XDG compatible paths for X software
    * Readline - Input configuration for common terminal utilities
    * Tmux - Terminal multiplexer
    * Git - Distributed VCS
    * Subversion - Centralized VCS
* Desktop tooling
    * *NOTE:* None of this is implemented, eventually want to switch to them
    * BSPWM - Lightweight tiling WM
    * Polybar - Customizable traybar
    * SXHKD - Simple hotkey demon
    * Rofi - Customizable launcher
    * Firefox - Browser
    * WINE - Windows emulation layer
* Language tooling
    * Ruby
        * Gem
        * Bundle
        * IRuby - Interpreter
    * Python
        * IPython
        * Jupyter
    * Javascript
        * NPM - Package management
    * Scheme
        * Racket (PLT Scheme)
    * C/C++
        * CCache
    * Rust
        * Rustup
        * Cargo
    * Java
        * Gradle
    * Haskell
        * Stack
    * Go
    * .NET
        * NuGet
* Misc.
    * GTK
    * KDE

And also offers the following themes for all desired software:
* Halogen - Mishmash of my own making. Warm colours. Primarily light solarized
  based

## Theming
We attempt to keep a global theme set via theme customisation using a themes
from `themes/$THEME_NAME`. The aim of this is to allow consistent theming.

Applications with complex configuration such as emacs or vim have toggles
(light/dark) and local installation customisation support, and as such will
not be customised in the global theme for the purposes of simplicity.

## Install Instructions
```shell
cd directory-of-this-repo
make make-directories link
```

Configuration installation can be customised via setting custom variables
for make. For example:
```shell
SOURCE_DIR=$DIRECTORY_OF_REPO/ \
  TARGET_DIR=$CUSTOM-HOME/ \
  PACKAGES="$PACKAGES $THEME_NAME" \
  make create-directories link
```

Configuration symlinks can be uninstalled via:
```shell
cd $DIRECTORY_OF_REPO
make unlink
```

Exactly one theme must be specified in order for the packages to work, as they
rely on loading from files only provided by the themes.

See the makefile for more targets and customization points.

### Shells

The `shells/` package is designed in order to allow for sharing as much posix
compatible configuration as possible (e.g. aliases and environment variables).
This shared configuration goes in
`$XDG_CONFIG_HOME/shell/{env,interactive,login,logout}`.
As one may expect from the naming convention, all shells load `env`, login
shells load `login`, interactive shells run `interactive`, and login shells run
`logout` on termination/logout.

Generally the following rules of thumb apply (although not complete):
It should go in `login` if:
* It's an exported variable/function.
* It needs to occur on initial login (e.g. display machine information).

It should go in `logout` if:
* It needs to happen on logout (usually cleanup of some kind).

It should go in `env` if:
* It needs to be available to all shells of all types.

It should go in `interactive` if:
* It's an alias.
* It's a shell session specific environment variable (not exported).

Top-level {.env,.interactive,.login,.logout} sh compatible scripts can be
placed in the subdirectories under `$XDG_CONFIG_HOME` and will be loaded
automatically at the appropriate stage. This is in order to allow application
specific shell configuration for more fine grained installation of these
dotfiles (for example: tmux aliases could be placed in
`$XDG_CONFIG_HOME/tmux/.interactive`, which then only be installed/linked and
loaded by the shell if tmux is wanted).

The configuration layout is designed so that scripts in shell specific
subdirectories are run when the user would normally expect, even if this
requires working around shell idiosyncrasies. Top-level shell scripts should
*not* need to be touched **ever**, and it is recommended that you don't, as the
workarounds needed to get the shells to load in a consistent order are
potentially fragile. All changes should be made to the files in the appropriate
subdirectories. Prefer to make the changes as general as possible.

Running non-interactive non-login shells with sh/ksh will cause nothing you
would ordinarily expect to get loaded as they do not load any scripts on start.
There is no known way of working around this. Both bash and zsh will always
load their respective `env` at a minimum, and ksh/sh will load `env` when they
are at least an interactive shell.

*NOTE: May jettison this massivley complex & fragile setup entirely for zsh
only configuration for the sake of simplification in future. May also change
env/interactive/login/logout distinction to something simpler like env/aliases
that always get loaded in all shell types if the former distinction between
login/non-login shells don't turn out to be useful. Beware*

*NOTE: Some Linux distros may break this separation of runtime entirely by
souring .bashrc in /etc/profile etc. If this ends up being the case, it may be
worth removing the separation of concerns entirely as mentioned in the prior
note.*

### Vim
Vim should setup default plugins via git & curl, and then configure itself when
first run.

Manual steps:

 * Start Vim.
 * Run `:PlugUpgrade`and then `:PlugUpdate`.
 * Run `:PlugInstall`.
 * Exit Vim

Alternatively, to do setup all in batch form run:
```shell
vim +PlugUpgrade +PlugUpdate +qall
```

The Vim configuration is split into a minimal no-plugin core and a default
medium-weight plugin using setup. Minimal vim can be started by setting the
environment variable `$VIM_MINIMAL` prior to launch, or using the vim-minimal
alias provided by the `shells` package.

Local machine plugin additions and overrides (not configuration) should be
placed in `$XDG_CONFIG_HOME/vim/custom.plugin.vim`. The allowed operations
are:

* `core#PluginAdd(name)`, where name is the `$owner/$repo` github subpath.
* `core#PluginPostUpdateHook(name, action)`, where `action` is a string of
  either a colon prefixed vim command, or a shell command.
* `core#PluginDisable(name)`
* `core#PluginPin(name, commit\_hash)`, where the hash is the specific commit
  hash desired to pin to (can be both long or short).
* `core#PluginUnpin(name)`

Local machine configuration additions (including configuration for plugins
specified in custom.plugins.vim) should be placed in
`$XDG_CONFIG_HOME/vim/custom.config.vim`. The provided operations are:

* core#PluginIsLoaded(name)

Typically our core vim functions for configuration are prefixed by `core#`.

The current load order of these scripts is as follows:
* `core.vim`
* If not minimal mode:
  * `plugin.vim`
  * `custom.plugin.vim`
* `custom.config.vim`

These scripts do not expose the ability to lazy load plugins despite the
underlying vim plugin manager `vim-plug` allowing for it, as this causes huge
issues with allowing fully flexible customization on a local install via the
`custom.*.vim` customization points (it likely requires a more powerful
dependency graph mechanism along with delayed execution hooks which are not
provided).

### Emacs
In the process of writing a full highly customised config.  Will use
general.el, which-key, evil, straight and Ivy?Helm?. The theme will togglable
between light & dark. Package usage and configuration will be locally
extendable and fully overridable. Both emacs and vim bindings for every package
will be maintained, and switching between a full set of emacs bindings vs vim
bindings will be enabled via a custom argument to emacs.

### Xresources & Xresources configured/themed applications
You must write an xinitrc or xsession loading the themes via xrdb. Themes
provide their own `.Xresources` that should be loaded in order to enjoy their
full benefits.

## Usage Instructions
The following configurations support override configs for the current user:
 * Vim (`$XDG_CONFIG_HOME/vim/custom.{config,plugin}.vim`)
 * Tmux (`$XDG_CONFIG_HOME/tmux/custom.conf`)
 * Shells (`$XDG_CONFIG_HOME/{shell,sh,bash,zsh,ksh}/custom.{env,interactive,login,logout}`)

By adding your own versions of the files listed above, you may stack
your own customisations on top of the ones provided. Typically one should
prefer making the base configuarations more configurable without manual
local configuration overrides the more complex the changes are.

Generally package paths for things like emacs and vim are directed into an
appropriate `~/.cache/vim` or `~/.cache/emacs` directory in order to avoid
changes in the tree of this repo's clone wherever possible. `~/.local/`
contains a `bin` directory that is automatically added to `PATH` for temporary
or personal binaries.

Generally config paths for things are stored in ~/.config (i.e. we follow XDG
layout wherever feasible, but this is made extremely hard by various project
owners).

## Extension
### Configurations
 * Add the appropriate package entry to `Makefile`.
 * Make a folder just to store this set of configs in the repo directory.
 * Add an appropriate .stow-local-ignore to that directory.
 * Add appropriate lines to the .gitignore in the repo directory.
### Themes
 * Copy an existing theme from the `themes/` directory.
 * Modify all the files to provide the colour-schemes, fonts and iconography
   you desire (all files are needed, as some applications assume their presence
   at the given directories)

