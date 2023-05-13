# My Dotfiles
A repo for storing my personal configuration files for software development
under Linux.

***Not really intended for public consumption***

## Table of Contents
* [Philosophy](#philosophy)
* [Future Goals](#future-goals)
* [Prerequisites](#prerequisites)
* [Configurations](#configurations)
* [Directory Structure](#directory-structure)
* [Theming](#theming)
* [Usage Instructions](#usage-instructions)
  * [Shells](#shells)
  * [Vim](#vim)
  * [Emacs](#emacs)
  * [Tmux](#tmux)
  * [Xresources applications](#xresources-applications)
* [Extension](#extension)
  * [Configurations](#configurations-1)
  * [Themes](#themes)

## Philosophy
The aim is to:

* Be modular. Configuration for individual applications should be as separate
  as possible, and one application should still work (to some extent) without
  another piece of configuration.
* Follow XDG layout where possible, even if that requires forcing applications
  to do so via hackery.
* Prefer commonly available tools where possible. We have no pretense of
  supporting non-Linux platforms, but at the very least we should try.
  Modularity should help.
* Be highly customizable at a per-machine level. This means providing
  consistent and simple override points such as many potentially broken aspects
  in the base layer can be turned off, and extra environment specific
  functionality can be shoehorned in, as reasonably possible (Things which are
  fundamentally broken should be fixed in the base layer, but local overrides
  should allow for stop-gap measures).
* Be reproducible. Wherever possible, and whenever external packages/plugins
  are needed, we must make it reproducible across both time and platform. This
  should be done via mechanisms such as package pinning to commit etc.
* Allow theming to be somewhat separate from application configuration (i.e.
  allow theming to be drop-in and reasonably globally consistent).

Unfortunately I require the ability to stow or symlink configuration files on
various Linux distros, so my dotfiles themselves are kept in a form making that
easy. If the requirement of supporting other distros
(and shells for that matter) could be dropped, it is likely that this could be
made considerably simpler, alas the world isn't so simple.

## Future Goals
* None currently

## Prerequisites
* Git (to clone this repo somewhere local).
* GNU Make (For automating running stow on the packages).
* GNU Stow (Though technically you can manually copy/symlink).

## Configurations
Currently includes some configuration for the following pieces of software:

* Editors
  * Emacs - Preferred highly customized IDE experience
  * Vim - Lightweight simple text editing experience
* Shells (These are merged into a single "package" with shared configuration)
  * Bash - Common on Linux
  * Ksh - Common on BSDs and old Unices
  * Sh - Common everywhere (Unix-likes)
  * Zsh - Preferred daily driver
* Terminals
  * URxvt - Lightweight, but lacks good support for a lot of features
  * XTerm - Ubiquitous, but also lacking in some features
* Dev tooling
  * Ack - Classic search tool
  * Aspell - Spelling aid
  * Docker - Containerization
  * Direnv - Environment variable daemon
  * Git - Distributed VCS
  * GnuPG - Signing and encryption tools
  * Less - Pager that's better than the basic `more` pager
  * Readline - Input configuration for common terminal utilities
  * Subversion - Centralized VCS
  * Terminfo - Terminal information querying tools
  * Tmux - Terminal multiplexer
  * Vagrant - Virtualization
  * WGet - Fetch files over the internet
  * Xorg utilities - Sets up XDG compatible paths for X software
* User & desktop applications
  * MPV - Media player that knocks the socks off of the competition
  * WINE - Windows emulation layer
  * Redshift - Monitor time/location-based color temperature adjustments
  * Feh - Minimal image viewer
  * Mozc - Open Google IME-based input method
  * Rclone - Remote filesystem mounting and syncing tool supporting many
    providers.
* Desktop environments (Not enabled by default)
  * None as of yet. I tend to just use Gnome because I'm lazy.
* Language tooling
  * .NET
    * NuGet - Package management
  * C/C++
    * CCache - Caching compilation by-products
  * Go
  * Haskell
    * Stack - Package management & build tool
  * Java
    * Java - The runtime itself
    * Gradle - Build tool
  * Javascript
    * NPM - Package management
    * Node - Javascript framework/environment
  * Python
    * IPython - Interpreter
    * Jupyter - Jupyter notebooks
  * Ruby
    * Bundle - Package management
    * Gem - Package management
    * IRuby - Interpreter
  * Rust
    * Cargo - Package management & build tool
    * Rustup - Installing specific rustc toolchains
* Misc.
  * GTK - Gnome GUI toolkit
  * KDE- KDE (and Qt) configuration
  * Nvidia - configuration and fixes for nvidia drivers/devices

And also offers the following themes for all desired software:
* Halogen - Mishmash of my own making. Warm colors. Primarily light solarized
  based.

## Directory Structure
Configuration is placed within a "package" subdirectory. The files in these
directories will mimic the final structure in the target directory if GNU Stow
were to be run upon them. Configuration for multiple applications that fall
into a single logical category (for example: `javascript` for all JS related
software) will be placed in the same "package".

Generally following XDG standards where possible, when installed the directory
structure will look like:

* `$TARGET/` - Usually `$HOME`
  * `.*` - Unfortunately loads of tools still don't support XDG standards
  * `.local/` - Recommended place to put custom scripts & `make install`
    results
    * `{bin,lib,include,etc}/`
    * `share/` - Where non-ephemeral data should be placed
  * `.config/` - Where static configuration scripts should be put
    * `$APPLICATION/`
      * `.{env,login,interactive,logout}`
      * `$CONFIG_FILES`
  * `.cache/` - Where files that can be regenerated are placed (ephemeral data)
    * `$APPLICATION/`
      * `$CACHE_FILES`

## Theming
We attempt to keep a global theme set via theme customization using a themes
from `themes/$THEME_NAME`. The aim of this is to allow consistent theming.

Applications with complex configuration such as emacs or vim have toggles
(light/dark) and local installation customization support, and as such will
not be customized in the global theme for the purposes of simplicity.

Typically various applications will need to load theming configuration files
from their base configuration file (or otherwise generally assume they're
present). Where possible we will check for the presence of files before
attempting to load them, but in some instances this isn't easily possible (e.g.
Xresources configuration). In other words, various package-specific
configuration will assume the presence of theming files that will only be
installed along with one of the provided themes. As such, it is recommended to
follow the structure of an existing theme when creating your own.
Alternatively, fork this project.

Currently using GNU Stow for installation it isn't easily possible to only
install theming files for installed packages (e.g. Xresources for XTerm only if
XTerm configuration is installed). This is possible using a more powerful
system such as Nix home-manager, or a script of some kind. This means that if
you use a simplistic symlink manager like Stow for installation you may end up
with stray theme files in your `$HOME` for applications for which you have not
installed other configuration for.

## Usage Instructions
```shell
cd directory-of-this-repo
make create-directories link
```

Configuration installation can be customized via setting custom variables
for make. For example:
```shell
make create-directories link \
  SOURCE_DIR=$DIRECTORY_OF_REPO/ \
  TARGET_DIR=$CUSTOM-HOME/ \
  PACKAGES="$PACKAGES $THEME_NAME"
```

Configuration symlinks can be uninstalled via:
```shell
cd $DIRECTORY_OF_REPO
make unlink
```

Exactly one theme must be specified in order for the packages to work, as they
rely on loading from files only provided by the themes.

See the makefile for more targets and customization points.

---

### Shells

The `shells/` package is designed in order to allow for sharing as much posix
compatible configuration as possible (e.g. aliases and environment variables).
This shared configuration goes in
`$XDG_CONFIG_HOME/shell/{env,interactive,login,logout}`.  As one may expect
from the naming convention, all shells load `env`, login shells load `login`,
interactive shells run `interactive`, and login shells run `logout` on
termination/logout.

Generally the following rules of thumb apply:
It should go in `login` if:
* It's an exported variable/function. Note that many user session variables
  should be preferably place within a `[0-9]+-*.conf` formatted file under
  `${XDG_CONFIG_HOME}/environment.d` so that they are correctly available to
  both systemd services and GUI applications even under Wayland sessions.
  However, if the environment variable is only meant to be used interactively
  within a terminal, you should prefer the `.interactive` mechanism for setting
  that variable (as a non-export).
* It needs to occur on initial login (e.g. display machine information).

It should go in `logout` if:
* It needs to happen on logout (usually cleanup of some kind).

It should go in `env` if:
* It needs to be available to all shells of all types.

It should go in `interactive` if:
* It's an alias.
* It's a shell session specific environment variable (not exported).

Shells follows a standard `conf.d/`-style alphanumerically ordered module
auto-loading system. The following load order is respected for the relevant :
* `$XDG_CONFIG_HOME/shell/modules/{env,login,interactive,logout}/*.sh`
* One of the following dependent on which shell is being configured:
  * `$XDG_CONFIG_HOME/sh/modules/{env,login,interactive,logout}/*.sh`
  * `$XDG_CONFIG_HOME/bash/modules/{env,login,interactive,logout}/*.sh`
  * `$XDG_CONFIG_HOME/ksh/modules/{env,login,interactive,logout}/*.sh`
  * `$XDG_CONFIG_HOME/zsh/modules/{env,login,interactive,logout}/*.sh`

This mechanism application-specific shell configuration allows for more modular
shell configuration. For example:
 * Tmux aliases could be placed in
   `$XDG_CONFIG_HOME/shell/modules/interactive/50-tmux.sh`.
 * These aliases will only be installed/linked and loaded by the shell if the
   tmux configuration is also copied/linked into that directory.
 * If the user does not choose to install tmux configuration, it will not be
   loaded at all.

Shell modules should assume as little knowledge of other shell modules as
possible in order to maximize isolation.

Due to the complexity of working around various shells' idiosyncrasies in order
to create a normalized set of customization points (env, login, interactive,
logout), top-level shell scripts (i.e. in `$HOME`) should be modified as little
as possible.

#### WARNING: Different Behavior of Non-Login Non-Interactive Shells
Running non-interactive non-login shells with sh/ksh will not load any
configuration. Bash/zsh will always load their respective `env` at a minimum in
all shell types, however sh/ksh must be at least an interactive shell to load
env.

---

### Vim

The Vim configuration deliberately eschews the use of plugins. This used to not
be the case - in the past Vim was the primary editor of these dotfiles and was
heavily customized. Currently Emacs is preferred, with Vim being the primary
workhorse for small and fast edits. As a result, the aim of Vim configuration
here is to configure sensible defaults and useful keybindings, all done with
minimal major deviations from the default behavior or extensions in
functionality.

If a specific installation decides plugins or customizations are needed, they
can be added in the standard Vim way via native package loading (i.e. the `pack`
directory`. See `:help packages` for upstream details.

#### Theming
This configuration has a togglable dual light/dark theme setup. The core
functions are:

* Setting dark and light themes via `let g:theme_dark='themename'` or
  `let g:theme_light='themename'`.
* `core#ThemeUpdate()`, which updates the currently used theme to reflect the
  current mode (light/dark) and above set themes (`g:theme_{light,dark}`).
* `core#ThemeToggle(target_theme)`, where `target_theme` is an optional
  parameter for the `'dark'` or `'light'` string. If `target_theme` is not
  provided, then this will behave as a simple toggle.
* `core#FontSetGui(font_dicts)`, where `font_dicts` is a list of one or more
  dictionaries containing an unformatted font name string and size number. The
  function will handle escaping spaces and platform differences in setting
  `&guifont`. The fonts will be tried in the specified order. Subsequent calls
  will erase previous font setting changes totally.
* `core#FontChangeSize(size)`, where size is the point size to change the set
  font to.
* `core#FontIncreaseSize()`, which increases the font size by one.
* `core#FontDecreaseSize()`, which decreases the font size by one.

The configuration uses an in-built pair of themes for light and dark modes. It
will use Vim's inferred choice for `background` in order to determine the
default mode to launch into.

Note that the font altering functions will only work under gvim, as terminal
vim is entirely reliant on the terminal font. Additionally because the Gnome
team are geniuses and GTK uses best effort matching for font names, it will
never fail on the first font, and therefore no alternatives will be attempted.

#### WARNING: Reconnecting to Existing Process from New X Session Clipboard
Vim does not adjust to new `$DISPLAY` values, causing X clipboard support to
break. This issue can be resolved via running `:xrestore`. Under X11, there is
a timer running every 2 hours with this effect by default in the configuration.
This timer can be disabled via `timer_stop(g:timer_xrestore)`. The function
`core#XRestore()` will perform the xrestore also, though offers no additional
convenience.

---

### Emacs
#### Installation
Emacs should setup default plugins via git and built-in web capabilities, and
then configure itself when first run.

#### Overview
The Emacs configuration is split into two layers: A core layer and an optional
set of mirroring override points.

The current load order of these scripts is as follows:
* `$HOME/.emacs` (On older systems - deprecated - forwards to below)
* `$XDG_CONFIG_HOME/emacs/early-init.el` (On newer systems)
* `$XDG_CONFIG_HOME/emacs/init.el`
* `$XDG_CONFIG_HOME/emacs/core-package.el` and
  `$XDG_CONFIG_HOME/emacs/core-lock.el` for package versions.
* `$XDG_CONFIG_HOME/emacs/custom-package.el` (If available) and
  `$XDG_CONFIG_HOME/emacs/custom-lock.el` for package versions.
* `$XDG_CONFIG_HOME/emacs/core-config.el`
* `$XDG_CONFIG_HOME/emacs/custom-config.el` (If available)
* `$XDG_CONFIG_HOME/emacs/custom-customization.el` (If available)

#### General Configuration
Per-install configuration overrides should be placed in
`$XDG_CONFIG_HOME/emacs/custom-config.el`. This is where you should place all
machine-local customizations that are *not* package declarations (this means
use-package blocks for said packages *is* fair game here).

#### Package Configuration
Local machine package additions and overrides (not general configuration)
should be placed in `$XDG_CONFIG_HOME/emacs/custom-package.el`. The allowed
operations are:
* `(straight-use-package '(PACKAGE-NAME ...RECIPE...)`

It should be preferred that all packages are managed via explicit recipes as
opposed to using straight's automatic features in order to be fully
reproducible. This should also be done for transitive dependencies. Pinning
should be fully managed via straight's versioning functions (e.g.
`straight-fetch-all`, `straight-freeze-versions` and `straight-thaw-versions`.

All custom package specifications should be done under the `'custom` straight
profile (i.e. `straight-current-profile` should be set to `'custom`).

Local machine configuration additions (including configuration for packages
declared in custom-package.el) should be placed in
`$XDG_CONFIG_HOME/emacs/custom-config.el`. The provided operations are:
* `(use-package PACKAGE-NAME ...CONFIGURATION-BODY...)`, where this follows
  typical third-party use-package usage.

With `use-package`, we prefer to be as explicit as possible, and to design under
the assumption that any package can be lazily loaded (which may not happen in
practice due to inter-package dependencies, but the `use-package` blocks should
be written with that in mind). We also prefer hooks + buffer-local modes to
global modes in almost all cases.

We provide three core hooks taking the frame being created for customizing frame
appearance:
 * `hgs-frame-customization-hook`, which is run always, regardless of Emacs
   flavor.
 * `hgs-frame-customization-gui-hook`, which is run only for GUI Emacs.
 * `hgs-frame-customization-tui-hook`, which is run only for terminal Emacs.

Typically our core emacs functions for configuration are prefixed by either
`hgs-` or `minmacs-`.

---

### Tmux
#### Overview
This configuration errs on the side of changing as few of the default
keybindings as possible. See the top of `tmux.conf` for an up-to-date
description of all custom bindings. `<Prefix> ?` will give a list of all
remaining default bindings, and `<Prefix> <Tab>` will give a list of all
bindings in the session.

Nested Tmux sessions are handled by double entry of the prefix to send to the
inner Tmux session (e.g. `<Prefix> <Prefix> <OTHERBINDING>` will pass through
`OTHERBINDING` to the inner session, and `<Prefix> <BINDING>` will pass the
`BINDING` to the outer session as normal).

The prefix is `<C-z>` by default as this prevents the most binding conflicts in
typical terminal workflows, and C-z's default suspension functionality in the
shell is obviated by the introduction of Tmux itself.

This configuration provides various customization points. The current load
order of these scripts is as follows:
* `$XDG_CONFIG_HOME/tmux/tmux.conf`
* If either light/dark mode:
  * `$XDG_CONFIG_HOME/tmux/light.conf`
  * `$XDG_CONFIG_HOME/tmux/dark.conf`
* `$XDG_CONFIG_HOME/tmux/custom.tmux.conf`

#### Configuration
Tmux has a customization point at `$XDG_CONFIG_HOME/tmux/custom.conf` which is
loaded after all other pre-provided configuration.

#### Theming
Tmux theming uses the `$XDG_CONFIG_HOME/tmux/{dark,light}.conf` files to set
the color scheme.

The theme can be live toggled between light and dark.

##### Warning: Light & Dark Theming
As theming Tmux affects so little visually, the ability to provide both a light
and dark theme, as well as toggle between them while in a session may be
removed for simplicity.

---

### Xresources applications
You must write an `xinitrc` or `xsession` script for loading the themes via
`xrdb -merge $HOME/.Xresources`. Themes provide their own `.Xresources` that
indirectly load all relevant X theming & configuration files, and so should be
loaded in order to use the theme.

Using your `xinitrc`, `.Xdefaults` or `xsession` files one can add their own
customizations to X programs before/after deferring to those provided by the
files in this repository.

Currently the way the theme-provided `.Xresources` loads application
configuration/theming files requires them to always be present on the system.
As such, all X applications are non-modular and must be installed together
(see: `xapplications/`. It may be possible to work around this using a more
complex preprocessor with xrdb (e.g.  m4).

## Extension
### Configurations
 * Add the appropriate package entry to `Makefile`.
 * Make a folder just to store this set of configs in the repo directory.
 * Add an appropriate .stow-local-ignore to that directory.
 * Add appropriate lines to the .gitignore in the repo directory.

### Themes
 * Copy an existing theme from the `themes/` directory.
 * Modify all the files to provide the color-schemes, fonts and iconography
   you desire (all files are needed, as some applications assume their presence
   at the given directories).

