"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use VimPlug for plugin management
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Grab vimplug if not available using curl
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Call :PlugInstall to install, :PlugUpdate to update
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'kien/ctrlp.vim'

call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable filetype plugins
filetype plugin on
filetype indent on
" Set to use relative line mode
set relativenumber
set number
set linebreak " Break lines at word (requires Wrap lines)
set showbreak=~~~ " Wrap-broken line prefix
set textwidth=80 " Line wrap (number of cols)
set showmatch " Highlight matching brace
set nohlsearch " Don't highlight search results
set smartcase " Enable smart-case search
set ignorecase " Always case-insensitive
set incsearch " Searches for strings incrementally
set autoindent " Auto-indent new lines
set expandtab " Use spaces instead of tabs
set shiftwidth=4 " Number of auto-indent spaces
set smartindent " Enable smart-indent
set smarttab " Enable smart-tabs
set softtabstop=4 " Number of spaces per Tab
set ruler " Show row and column ruler information
set cmdheight=2 " Height of the command bar
set history=1000 " Sets how many lines of history VIM has to remember
set undolevels=1000 " Number of undo levels
set backspace=indent,eol,start " Backspace behaviour
set lazyredraw " Don't redraw while executing macros (good performance config)
set foldcolumn=1 " Add a bit extra margin to the left
set laststatus=2 " Always show the status line
" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=250
" Recursively upwards search for a ctags file till root
set tags=./tags,./TAGS,tags;/,TAGS;/

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors, themes and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T " no toolbar
  set guioptions-=e
  set guitablabel=%M\ %t
endif

" In case we haven't got the theme installed, just ignore errors
set background=light
try
  colorscheme solarized
catch
endtry

" Set utf8 as standard encoding
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup/swap off
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Change leader key to space
nnoremap <space> <leader>
let mapleader="\<space>"

" Map escape to kj for fast return to normal mode
inoremap kj <ESC>

" Fast saving
nnoremap <leader>ww :w!<cr>
" :W sudo saves the file
" (useful for handling the permission-denied error)
command W w !sudo tee % > /dev/null

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Faster way to manipulate windows (I can't deal with ^W start)
" Right/down/up/left
nnoremap <leader>wj <C-W>j
nnoremap <leader>wk <C-W>k
nnoremap <leader>wh <C-W>h
nnoremap <leader>wl <C-W>l
" Open splits
nnoremap <leader>wv :vsp<CR>
nnoremap <leader>ws :sp<CR>
" Close splits
nnoremap <leader>wc <C-W>c
nnoremap <leader>wo <C-W>o
" Move splits
nnoremap <leader>wJ <C-W>J
nnoremap <leader>wK <C-W>K
nnoremap <leader>wH <C-W>H
nnoremap <leader>wL <C-W>L

" Show buffer list and prompt for number/name for switching - b(uffer) j(ump)
nnoremap <leader>bj :ls<CR>:b<Space>
" Buffer navigation
set wildcharm=<C-z>
nnoremap <leader>b :buffer <C-z><S-Tab> " Search buffers by name
nnoremap <leader>B :sbuffer <C-z><S-Tab>
nnoremap <leader>bn :bn<CR> " Cycle back and forth between buffers
nnoremap <leader>bp :bp<CR>
nnoremap <leader>ba <C-^> " Switch to last open buffer

" Tag regex search
nnoremap <leader>j :tjump /

" Leave a mark on the current line of the buffer when leaving based
" on the language of the file contents. Use capital letters only.
augroup VIMRC
  autocmd!

  autocmd BufLeave *.C,*.H,*.c,*.h,*.cc,*.hh,*.c++,*.h++,*.cpp,*.hpp normal! mC
  autocmd BufLeave *.sh normal! mB
  autocmd BufLeave *.py normal! mP
  autocmd BufLeave *.rb normal! mR
augroup END

" Delete trailing white space on save
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite * :call DeleteTrailingWS()

" Use tabs instead of spaces in makefiles
autocmd FileType make setlocal noexpandtab

" Load a user-defined custom overrides file if it exists
if filereadable("~/.vimrc.local")
  source ~/.vimrc.local
endif

