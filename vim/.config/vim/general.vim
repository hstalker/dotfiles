set ffs=unix,dos,mac
let defaulttabsize = 2 " Sets all the tab options appropriately later on
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
set laststatus=2 " Always show the status line
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=250
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
set wildignorecase " Make searches case-insensitive

" Fold configuration
set foldmethod=indent " Use indent level to auto fold
set foldnestmax=10
" set nofoldenable " Disable automatic folding on buffer open
set foldlevel=0 " Default nesting to automatically open folds to
" We don't want folds to be closed by default, especially for which-key
set foldlevelstart=99 

" Turn backup/swap off
set nobackup
set nowb
set noswapfile

" Change netrwhist directory
let g:netrw_home=g:vim_cache_dir

" Default updatetime of 4000ms is bad for async updates
set updatetime=100

" Display invisible characters in the editor
set showbreak=↪\
set listchars=tab:→\ ,eol:↲,nbsp:␣,trail:·,extends:⟩,precedes:⟨
set list " Necessary to force display

fun! TrimTrailingWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

" Manually change tab size in current session
func! ChangeTabs(size)
  set autoindent " Auto-indent new lines
  set expandtab " Use spaces instead of tabs
  set smartindent " Enable smart-indent
  set smarttab " Enable smart-tabs
  let &shiftwidth=a:size " Number of auto-indent spaces
  let &tabstop=a:size " Size appearance of \t in text
  set softtabstop=0 " Number of spaces per Tab
endfunc

call ChangeTabs(defaulttabsize)

" Use tabs instead of spaces in makefiles
augroup MAKEFILE
  autocmd! MAKEFILE
  autocmd FileType make setlocal noexpandtab
augroup END

