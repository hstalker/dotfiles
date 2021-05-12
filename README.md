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
easy. Current plans are to move to a NixOS configuration, and use these
dotfiles as-is via home-manager. If the requirement of supporting other distros
(and shells for that matter) could be dropped, it is likely that this could be
made considerably simpler, alas the world isn't so simple.

## Future Goals
* Try to move Vim plug usage to support an automated lockfile mechanism for
  pinning.
* Consider move to NixOS on all my systems, whilst maintaining near full
  stowability of dotfiles to non-Nix systems.

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
* Desktop tooling
  * MPV - Media player that knocks the socks off of the competition
  * WINE - Windows emulation layer
  * Redshift - Monitor time/location-based color temperature adjustments
  * Mozc - Open Google IME-based input method
* Desktop environments (Not enabled by default)
  * Sway/Wofi/Waybar/Swaylock/Swaybg/Mako - Tiling Wayland native desktop
    environment.
* Language tooling
  * .NET
    * NuGet - Package management
  * C/C++
    * CCache - Caching compilation by-products
  * Go
  * Haskell
    * Stack - Package management & build tool
  * Java
    * Gradle - Build tool
  * Javascript
    * NPM - Package management
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
`$XDG_CONFIG_HOME/shell/{env,interactive,login,logout}`.
As one may expect from the naming convention, all shells load `env`, login
shells load `login`, interactive shells run `interactive`, and login shells run
`logout` on termination/logout.

Generally the following rules of thumb apply:
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
automatically at the appropriate stage in subdirectory alphabetical order. This
is in order to allow application specific shell configuration for more fine
grained installation of these dotfiles (for example: tmux aliases could be
placed in `$XDG_CONFIG_HOME/tmux/.interactive`, which then will only be
installed/linked and loaded by the shell if the tmux configuration is
installed).

Shells will load per-install per-shell override scripts as per the prior
mentioned rules. These customization points can be found at:
`$XDG_CONFIG_HOME/{shell,sh,bash,zsh,ksh}/custom.{env,interactive,login,logout}`.

Due to the complexity of working around various shells' idiosyncrasies.
Top-level shell scripts (i.e. in `$HOME`) should not need to be modified.

#### Misc: Different Behavior of Non-Login Non-Interactive Shells
Running non-interactive non-login shells with sh/ksh will not load any
configuration. Bash/zsh will always load their respective `env` at a minimum in
all shell types, however sh/ksh must be at least an interactive shell to load
env.

#### Warnings: Linux Distributions May Break the Setup
It is possible that certain Linux distributions may source shell configuration
files in their base scripts despite that altering the standard loading
behavior. This would break our design. As far as we are aware, all the most
common distros don't do this however.

#### Warnings: This Design is Brittle and Subject to Change
This setup works under the assumption that all shells are bourne shells
(doesn't take into account sharing configuration to cshell derivatives or
eshell for instance), and is only useful in a scenario where one changes shells
often. This makes it both limited in scope, and unlikely to really be useful,
especially if one writes their shell configuration in as sh compatible style as
possible. Additionally the way this setup works in convoluted and brittle. As
such it is a valid concern that this setup may be jettisoned entirely to focus
on supporting a single shell type, vastly reducing the overhead.

---

### Vim
#### Installation
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

#### Overview
The Vim configuration is split into a minimal no-plugin core and a default
medium-weight plugin using setup. Minimal vim can be started by setting the
environment variable `$VIM_MINIMAL` prior to launch, or using the vim-minimal
alias provided by the `shells` package.

This configuration provides various customization points. The current load
order of these scripts is as follows:
* `$XDG_CONFIG_HOME/vim/core.vim`
* If not minimal mode:
  * `$XDG_CONFIG_HOME/vim/plugin.vim`
  * `$XDG_CONFIG_HOME/vim/custom.plugin.vim`
* `$XDG_CONFIG_HOME/vim/custom.config.vim`

#### General Configuration
Per-install configuration overrides should be placed in
`$XDG_CONFIG_HOME/vim/custom.config.vim`. This is where you should place all
machine-local customizations that are *not* plugin related (think mappings,
functions, tab settings etc.).

#### Plugin Configuration
Local machine plugin additions and overrides (not general configuration) should
be placed in `$XDG_CONFIG_HOME/vim/custom.plugin.vim`. The allowed operations
are:

* `core#PluginAdd(name)`, where name is the `$owner/$repo` github subpath.
* `core#PluginPostUpdateHook(name, action)`, where `action` is a string of
  either a colon prefixed vim command, or a shell command.
* `core#PluginDisable(name)`
* `core#PluginPin(name, commit_hash)`, where the hash is the specific commit
  hash desired to pin to (can be both long or short).
* `core#PluginUnpin(name)`

Local machine configuration additions (including configuration for plugins
specified in custom.plugin.vim) should be placed in
`$XDG_CONFIG_HOME/vim/custom.config.vim`. The provided operations are:

* `core#PluginIsLoaded(name)`

Typically our core vim functions for configuration are prefixed by `core#`.

##### Example: Custom Per-Install Overrides
An example `custom.plugin.vim`:
```vimscript
" Disable the auto-save plugin because it causes issues on our machine
core#PluginDisable('907th/vim-auto-save')
" Allow vim-plug to grab the latest commit in master of vim-airline
core#PluginUnpin('vim-airline/vim-airline')
" We want syntax support for more languages because we use them on this machine
core#PluginAdd('vim-polyglot')
" We want to use this known, working commit for vim-polyglot
core#PluginPin('vim-polyglot', '9c3c0bc082e0d58d15dc6f24d8a335931417e2f0')
```

An example `custom.config.vim`:
```vimscript
" Change the airline theme
if core#PluginIsLoaded('vim-airline/vim-airline')
  if core#PluginIsLoaded('vim-airline/vim-airline-themes')
    let g:airline_theme='zenburn'
  endif
endif

" We want F5 compiles like an IDE :-)
nnoremap <F5> :make<CR>
```

#### Theming
This configuration has a togglable dual light/dark theme setup. The core
functions are:

* Setting dark and light themes via `let g:theme_dark='themename'` or `let
  g:theme_light='themename'`.
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

The configuration uses an in-built pair of themes for light and dark modes when
run in minimal mode, and uses a solarized pair of truecolor themes when plugins
are enabled by default. It will use Vim's inferred choice for `background` in
order to determine the default mode to launch into.

Note that the font altering functions will only work under gvim, as terminal
vim is entirely reliant on the terminal font. Additionally because the Gnome
team are geniuses and GTK uses best effort matching for font names, it will
never fail on the first font, and therefore no alternatives will be attempted.

#### Misc: Lazy Loading
These scripts do not expose the ability to lazy load plugins despite the
underlying vim plugin manager `vim-plug` allowing for it, as this causes issues
with allowing fully flexible customization on a local install via the
`custom.*.vim` customization points (Allowing for lazy-loading while retaining
the ability to fully customize the configuration per-install likely requires a
more powerful dependency graph mechanism, along with delayed execution hooks,
both of which are not provided in this implementation for simplicity).

#### Misc: Reconnecting to Existing Process from New X Session Clipboard Issues
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
* `$XDG_CONFIG_HOME/emacs/custom-abbreviation.el` (If available)

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

The prefix is `<C-SPC>` by default.

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

