scriptencoding utf-8
set nocompatible
" Was vim compiled with unicode support?
if has('multi_byte')
  " How your terminal encodes your input (from locale)
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  " The encoding used in vim's internal representation.
  set encoding=utf-8
  " The default encoding to use in buffers for the file on disk. Is buffer
  " local by default, so use setglobal to set for all buffers.
  setglobal fileencoding=utf-8
  " The heuristic list of encodings to try (in order) setting
  " fileencoding to.  Will use the first one that doesn't cause errors.
  " UCS-BOM will never get used if after UTF-8.
  set fileencodings=ucs-bom,utf-8,default,latin1
  " setglobal bomb " Uncomment to force a byte order mark for all buffers
endif
" We want these off until after plugins have all been loaded
filetype indent plugin off
syntax off


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setup core vim directory variables
if !empty($XDG_CONFIG_HOME)
  let g:config_dir=expand('$XDG_CONFIG_HOME/vim/')
else
  let g:config_dir=expand('$HOME/.config/vim/')
endif
if !empty($XDG_CACHE_HOME)
  let g:cache_dir=expand('$XDG_CACHE_HOME/vim/')
else
  let g:cache_dir=expand('$HOME/.cache/vim/')
endif
if !empty($XDG_DATA_HOME)
  let g:data_dir=expand('$XDG_DATA_HOME/vim/')
else
  let g:data_dir=expand('$HOME/.local/share/vim/')
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Check for if we're running in minimal mode (core only, no plugins)
if empty($VIM_MINIMAL)
  let g:minimal_mode=0
else
  let g:minimal_mode=1
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setup theming defaults
" We have a light/dark theme toggling system
" For truecolour support under newer version of vim
" This will have issues under certain terminals like rxvt. Xterm should have
" no problem.
if has("termguicolors")
  set termguicolors
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
let g:theme_current_default=&background
let g:theme_dark_default='evening'
let g:theme_light_default='morning'
let g:theme_current=g:theme_current_default
let g:theme_dark=g:theme_dark_default
let g:theme_light=g:theme_light_default


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets all the tab options appropriately later on
" Call UpdateTabs(n) to change tab width everywhere vim requires it
let g:tab_width_default = 2


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setup leader keys
" Change leader key to space
let g:mapleader = "\<Space>"
" For filetype specific leader bindings
let g:maplocalleader = ','


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" All the core settings
set ffs=unix,dos,mac " Set line ending order of preference
set relativenumber " Set to use relative line mode
set number " Set to use absolute number for current line only
set textwidth=0 " Hard line wrap (number of cols)
set wrapmargin=0
set wrap
set linebreak " Break lines at word (requires Wrap lines)
" set columns=80 " Soft line wrap (number of cols)
set showmatch " Highlight matching brace
set nohlsearch " Don't highlight search results
set smartcase " Enable smart-case search
set ignorecase " Always case-insensitive
set incsearch " Searches for strings incrementally
set ruler " Show row and column ruler information
set cmdheight=2 " Height of the command bar
set history=1000 " Sets how many lines of history VIM has to remember
set undolevels=1000 " Number of undo levels
set backspace=indent,eol,start " Backspace behaviour
set lazyredraw " Don't redraw while executing macros (good performance config)
set foldcolumn=4 " Add a bit extra margin to the left
set laststatus=2 " Always show the status line.
" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=250
" Enable mouse
set mouse=a
" For interfacing with the system clipboard via unnamed register by default
if has('clipboard')
  if has('unnamedplus')
    set clipboard^=unnamed,unnamedplus
  else
    set clipboard^=unnamed
  endif
endif

" Ignore files in in-built search
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,
set wildignore+=*.sln,*.Master,*.csproj,*.csproj.user,*.cache
set wildignore+=*.dll,*.dll.a,*.a,*.pdb,*.min.*,.DS_Store
set wildignore+=*.o,*.obj
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags
set wildignore+=*.tar.*
set wildignorecase " Make searches case-insensitive.

" Fold configuration
set foldmethod=indent " Use indent level to auto fold.
set foldnestmax=10
" set nofoldenable " Disable automatic folding on buffer open
set foldlevel=0 " Default nesting to automatically open folds to.
" We don't want folds to be closed by default, especially for which-key
set foldlevelstart=99

" Change netrwhist directory
let g:netrw_home=g:cache_dir

" Set swap/backup/undo/viminfo/rtp based on our base paths
for missing_directory in ['swap', 'backup', 'backup']
  if !isdirectory(g:cache_dir . missing_directory)
    call mkdir(g:cache_dir . missing_directory)
  endif
endfor
execute 'set directory=' . g:cache_dir . 'swap,~/,/tmp'
execute 'set backupdir=' . g:cache_dir . 'backup,~/,/tmp'
execute 'set undodir=' . g:cache_dir . 'undo,~/,/tmp'
execute 'set viminfo+=n' . g:cache_dir . 'viminfo'
execute 'set runtimepath+=' . g:config_dir . ','
  \ . g:config_dir . 'after,'
  \ . '$VIM,$VIMRUNTIME'

" Default updatetime of 4000ms is bad for async updates
set updatetime=100

" Display invisible characters in the editor
" I prefer to use the unicode symbols, but often monospace fonts just don't
" support them, so we'll put up with the pure ascii + extended ascii for now,
" as they are supported by, say, consolas.
"set showbreak=↪\
" Displaying eol is probably not very helpful
"set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:·,extends:⟩,precedes:⟨
set showbreak=\\ " Visualize soft line breaks
set listchars=tab:..,nbsp:_,trail:·,precedes:«,extends:»
set list " Necessary to force display

" Tab configuration
set autoindent " Auto-indent new lines
set expandtab " Use spaces instead of tabs
set smartindent " Enable smart-indent
set smarttab " Enable smart-tabs
set softtabstop=0 " Number of spaces per Tab

" Use tabs instead of spaces in makefiles
augroup MAKEFILE
  autocmd! MAKEFILE
  autocmd FileType make setlocal noexpandtab
augroup END

" Map escape to kj for fast return to normal mode
inoremap kj <ESC>

" Automatically refresh X session details at a 2-hourly interval
function! core#XRestore(...)
  xrestore
endfunction
if has('timers') && has('x11')
  let g:timer_xrestore = timer_start(2 * 60 * 60 * 1000, 'core#XRestore',
    \ {'repeat': -1})
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Useful core functions
" Set guifont appropriately for multiple platforms, takes a list of
" dictionaries with name and size keys
function! core#FontSetGui(font_dicts)
  " Build up a guifont string from the ordered alternatives given via the list
  " of font dictionaries.
  let l:font_string = ''
  if len(a:font_dicts) == 0
    echoerr "Expected at least one font!"
  endif
  for l:font in a:font_dicts
    let l:name = font['name']
    let l:size = string(font['size'])
    " Escape spaces in font names
    let l:escaped_name = substitute(l:name, ' ', '\\ ', 'g')
    " Escape commas in font names
    let l:escaped_name = substitute(l:escaped_name, ',', '\\\\,', 'g')
    " Set fonts for different platforms and GUI toolkits
    if has("gui_gtk3") || has("gui_gtk2") || has('x11')
      " Linux - NOTE: GTK doesn't support fallbacks because of course
      let l:font_string = l:font_string . l:escaped_name
        \ . '\ ' . l:size
    elseif has("gui_macvim")
      " MacOS
      let l:font_string = l:font_string . l:escaped_name
        \ . ':h' . l:size
    elseif has("gui_win32")
      " Windows - c is character set, h is height, q is aa and cleartype
      " Can be set to bold, italic, underline or strikeout via b, i, u and s
      " respectively.
      let l:font_string = l:font_string . l:escaped_name
        \ . ':h' . l:size . ':cDEFAULT:qDEFAULT'
    else
      " No idea what gui we're using, so just wing it
      let l:font_string = l:font_string . l:escaped_name . l:size
    endif
    let l:font_string = l:font_string . ','
  endfor
  " Chop off trailing comma
  let l:font_string = l:font_string[:-2]
  execute 'set guifont=' . l:font_string
endfunction
command! -nargs=1 FontSetGui call core#FontSetGui(<args>)

" Allow dynamic font size alterations
function! core#FontChangeSize(size)
  let &guifont = substitute(&guifont, '[1-9][0-9]*', string(a:size), 'g')
endfunction
command! -nargs=1 FontChangeSize call core#FontChangeSize(<args>)

function! core#FontIncreaseSize(...)
  let l:increment = get(a:, 1, 1)
  let l:current_size = str2nr(substitute(&guifont,
    \ '^\(.* \)\([1-9][0-9]*\)', '\2', ''))
  call core#FontChangeSize(current_size + increment)
endfunction
command! -nargs=? FontIncreaseSize call core#FontIncreaseSize(<args>)

function! core#FontDecreaseSize(...)
  let l:decrement = -get(a:, 1, 1)
  call core#FontIncreaseSize(l:decrement)
endfunction
command! -nargs=? FontDecreaseSize call core#FontDecreaseSize(<args>)

" Attempt to source the given file if it's readable on the filesystem
function! core#TrySource(script_path)
  if filereadable(a:script_path)
    execute 'source ' . a:script_path
  endif
endfunction

" Update themes according to set light/dark theme selection
function! core#ThemeUpdate()
  try
    execute 'set background=' . g:theme_current
    if g:theme_current ==? 'light'
      execute 'colorscheme ' . g:theme_light
    elseif g:theme_current ==? 'dark'
      execute 'colorscheme ' . g:theme_dark
    endif
  catch
    execute 'set background=' . g:theme_current_default
    if g:theme_current ==? 'light'
      execute 'colorscheme ' . g:theme_dark_default
    elseif g:theme_current ==? 'dark'
      execute 'colorscheme ' . g:theme_light_default
    endif
    echom 'Preferred ' . g:theme_current . ' colour scheme not available'
  endtry
endfunction
command! ThemeUpdate call core#ThemeUpdate()

" For toggling between light and dark themes
function! core#ThemeToggle(...)
  let l:target_theme = get(a:, 1, 0)
  if l:target_theme =~? 'light\|dark'
    let g:theme_current=l:target_theme
  else
    if g:theme_current ==? 'light'
      let g:theme_current='dark'
    elseif g:theme_current ==? 'dark'
      let g:theme_current='light'
    else
      echom 'Unknown g:theme_current value '''
        \ . g:theme_current
        \ . '''. Setting it to ''light'' as default.'
      let g:theme_current='light'
    endif
  endif
  call core#ThemeUpdate()
endfunction
command! -nargs=? ThemeToggle call core#ThemeToggle(<args>)
command! ThemeLight call core#ThemeToggle('light')
command! ThemeDark call core#ThemeToggle('dark')

" Manually change tab size in current session
function! core#TabWidthChange(...)
  let l:target_size = get(a:, 1, 0)
  if !l:target_size
    let l:target_size = g:tab_width_default
  endif
  let &shiftwidth=l:target_size " Number of auto-indent spaces
  let &tabstop=l:target_size " Size appearance of \t in text
endfunction
command! -nargs=? TabWidthChange call core#TabWidthChange(<args>)
command! TabWidthReset call core#TabWidthChange(g:tab_width_default)

" Set the tabs and themes to their default state
call core#TabWidthChange()
call core#ThemeUpdate()


function! core#SyntaxHlToggle()
  if exists("g:syntax_on")
    syntax off
  else
    syntax enable
  endif
endfunction
command! SyntaxHlToggle call core#SyntaxHlToggle()

function! core#TrimTrailingWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfunction
command! TrimTrailingWhitespace call core#TrimTrailingWhitespace()
" We want vim to trim trailing whitespace before any save occurs
autocmd! BufWritePre * call core#TrimTrailingWhitespace()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom delayed loading plugin handling functions
" Allows a local install to customise package usage completely before
" attempting to load anything.

" Maintain a delayed registry of plugins to load to allow for custom overrides
let g:plugin_registry = {}


" Add a plugin to the delay load registry
function! core#PluginAdd(name)
  if !has_key(g:plugin_registry, a:name)
    let g:plugin_registry[a:name] = {'enabled': 1, 'loaded': 0}
  else
    echom 'Attempted to add already known plugin: ' . a:name
  endif
endfunction

" Disable a plugin in the delay load registry
function! core#PluginDisable(name)
  if has_key(g:plugin_registry, a:name)
    let g:plugin_registry[a:name]['enabled'] = 0
  else
    echom 'Attempted to disable unknown plugin: ' . a:name
  endif
endfunction

" Pin a plugin in the delay load registry to a specific commit SHA
function! core#PluginPin(name, commit_hash)
  " Check if plugin name exists, error if doesn't
  if has_key(g:plugin_registry, a:name)
    let g:plugin_registry[a:name]['commit'] = a:commit_hash
  else
    echom 'Attempted to pin to commit hash unknown plugin: ' . a:name
  endif
endfunction

" Run an action after an update occurs (e.g. compile a module)
function! core#PluginPostUpdateHook(name, operation)
  " Check if plugin name exists, error if doesn't
  if has_key(g:plugin_registry, a:name)
    let g:plugin_registry[a:name]['do'] = a:operation
  else
    echom 'Attempted to add post update hook to unknown plugin: ' . a:name
  endif
endfunction

" Unpin a previously pinned plugin in the delay load registry
function! core#PluginUnpin(name)
  if has_key(g:plugin_registry, a:name)
    remove(g:plugin_registry[a:name], 'commit')
  else
    echom 'Attempted to unpin unknown plugin: ' . a:name
  endif
endfunction

" Queries whether a given plugin has loaded
function! core#PluginIsLoaded(name)
  if has_key(g:plugin_registry, a:name)
    return g:plugin_registry[a:name]['loaded']
  else
    echom 'Attempted to query whether unknown plugin is loaded: ' . a:name
    return 0
  endif
endfunction

" Use the delay load plugin registry to prime vim-plug
function! core#PluginProcessRegistry()
  for [name, extra_options] in items(g:plugin_registry)
    if extra_options['enabled']
      try
        Plug name, extra_options
        let g:plugin_registry[name]['loaded'] = 1
      catch
        echom "Failed to load plugin: '" . name .  "'"
        let g:plugin_registry[name]['loaded'] = 0
      endtry
    endif
  endfor
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T " Toolbar
  set guioptions-=m " Menu bar
  set guioptions-=r " Right-hand scroll bar
  set guioptions-=R " When vertically split window?
  set guioptions-=l " Left-hand scroll bar
  set guioptions-=L " When vertically split window?
  set guioptions-=b " bottom horizontal scroll bar
  set guioptions+=e " Tab pages
  set guioptions+=g " Show inactive menu items as greyed out?
  set guioptions+=i " Use Vim icon
  set guioptions-=f " Should gvim fork-detach from parent shell?
  set guioptions+=v " Should prefer vertical button layout for dialogs?
  " Describes text to use in labels on GUI tab pages line
  set guitablabel=%M\ %t

  " Set default fonts
  call core#FontSetGui([
    \ {'name': 'DejaVu Sans Mono', 'size': 12},
    \ {'name': 'Consolas', 'size': 12},
    \ {'name': 'Menlo Regular', 'size': 12},
    \ ])
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Loading additional scripts
" Load plugins with configurations if not specified to run in minimal mode
if !g:minimal_mode
  execute 'source ' . g:config_dir . 'plugin.vim'
endif

" Load all plugin filetype indent, plugin and syntax scripts now
filetype indent plugin on
syntax on

" Load local installation customisations after everything else is done
call core#TrySource(g:config_dir . 'custom.config.vim')

