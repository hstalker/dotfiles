scriptencoding utf-8
set encoding=utf8
set nocompatible
filetype indent plugin on
syntax on

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

" We have a light/dark theme toggling system.
" Override the strings g:theme_dark and g:theme_light
" to select new colour schemes for vim.
let g:theme_current_default='light'
let g:theme_dark_default='torte'
let g:theme_light_default='desert'
let g:theme_current=g:theme_current_default
let g:theme_dark=g:theme_dark_default
let g:theme_light=g:theme_light_default

" Sets all the tab options appropriately later on.
" Call UpdateTabs(n) to change tab width everywhere vim requires it.
let g:tab_width_default = 2

" Change leader key to space.
let g:mapleader = "\<Space>"
" For filetype specific leader bindings.
let g:maplocalleader = ','

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
execute 'set viminfo+=n' . g:cache_dir . 'vim/viminfo'
" No annoying sound on errors.
set noerrorbells
set novisualbell
set t_vb=
set tm=250
" For interfacing with the system clipboard via unnamed register by default.
if has('clipboard')
  if has('unnamedplus')
    set clipboard^=unnamed,unnamedplus
  else
    set clipboard^=unnamed
  endif
endif

" Ignore files in in-built search.
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,
set wildignore+=*.sln,*.Master,*.csproj,*.csproj.user,*.cache
set wildignore+=*.dll,*.dll.a,*.a,*.pdb,*.min.*,.DS_Store
set wildignore+=*.o,*.obj
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags
set wildignore+=*.tar.*
set wildignorecase " Make searches case-insensitive.

" Fold configuration.
set foldmethod=indent " Use indent level to auto fold.
set foldnestmax=10
" set nofoldenable " Disable automatic folding on buffer open.
set foldlevel=0 " Default nesting to automatically open folds to.
" We don't want folds to be closed by default, especially for which-key.
set foldlevelstart=99

" Turn backup/swap off.
set nobackup
set nowb
set noswapfile

" Change netrwhist directory.
let g:netrw_home=g:cache_dir

" Default updatetime of 4000ms is bad for async updates.
set updatetime=100

" Display invisible characters in the editor.
set showbreak=↪\
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:·,extends:⟩,precedes:⟨
set list " Necessary to force display

" Use tabs instead of spaces in makefiles.
augroup MAKEFILE
  autocmd! MAKEFILE
  autocmd FileType make setlocal noexpandtab
augroup END

" Map escape to kj for fast return to normal mode.
imap kj <ESC>

" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T " no toolbar
  set guioptions-=e
  set guitablabel=%M\ %t
  set guifont=Consolas:12,DejaVuSansMono:12
endif

" Update themes according to set light/dark theme selection.
function ThemeUpdate()
  try
    execute 'set background=' . g:theme_current
    if g:theme_current == 'light'
      execute 'colorscheme ' . g:theme_light
    elseif g:theme_current == 'dark'
      execute 'colorscheme ' . g:theme_dark
    endif
  catch
    execute 'set background=' . g:theme_current_default
    if g:theme_current ==? 'light'
      execute 'colorscheme ' . g:theme_light
    elseif g:theme_current ==? 'dark'
      execute 'colorscheme ' . g:theme_dark
    endif
    echom 'Preferred ' . g:theme_current . ' colour scheme not available'
  endtry
endfunction
command ThemeUpdate call ThemeUpdate()

" Function for toggling between light and dark themes
function ThemeToggle(...)
  let l:target_theme = get(a:, 1, 0)
  if l:target_theme && (l:target_theme ==? 'light' || target_theme ==? 'dark')
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
  call ThemeUpdate()
endfunction
command -nargs=? ThemeToggle call ThemeToggle(<args>)
command ThemeLight call ThemeToggle('light')
command ThemeDark call ThemeToggle('dark')

" Manually change tab size in current session
function TabWidthChange(...)
  let l:target_size = get(a:, 1, 0)
  if !l:target_size
    let l:target_size = g:tab_width_default
  endif
  set autoindent " Auto-indent new lines
  set expandtab " Use spaces instead of tabs
  set smartindent " Enable smart-indent
  set smarttab " Enable smart-tabs
  let &shiftwidth=l:target_size " Number of auto-indent spaces
  let &tabstop=l:target_size " Size appearance of \t in text
  set softtabstop=0 " Number of spaces per Tab
endfunction
command -nargs=? TabWidthChange call TabWidthChange(<args>)
command TabWidthReset call TabWidthChange(g:tab_width_default)

call TabWidthChange()
call ThemeUpdate()

function SyntaxHlToggle()
  if exists("g:syntax_on")
    syntax off
  else
    syntax enable
  endif
endfunction
command SyntaxHlToggle call SyntaxHlToggle()

function TrimTrailingWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfunction
command TrimTrailingWhitespace call TrimTrailingWhitespace

" Load plugins with configurations if not specified to run in minimal mode.
if empty($VIM_MINIMAL)
  execute 'source ' . g:config_dir . 'plugins.vim'
endif

" Load local installation customisations after everything else is done.
if filereadable(g:config_dir . 'custom.vim')
  execute 'source ' . g:config_dir . 'custom.vim'
endif

