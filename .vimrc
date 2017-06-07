" Vim configuration - aim to keep minimalist


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use VimPlug for plugin management
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Call :PlugInstall to install, :PlugUpdate to update
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'rhysd/vim-clang-format'
Plug 'kien/rainbow_parentheses.vim'
Plug 'majutsushi/tagbar'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'ctrlpvim/ctrlp.vim'

call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable filetype plugins
filetype plugin on
filetype indent on
" Set to use hybrid line mode - absolute for current line, relative for all
" others
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
set autochdir " Change working directory to open buffer
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
set tm=500
" Recursively upwards search for a ctags file till root
set tags=./tags,./TAGS,tags;/,TAGS;/

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.obj,*.exe,*.dll,*.so,*.zip,*.tar.gz,*.swp
if has("win16") || has("win32")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
    set wildignore+=.git\*,.hg\*,.svn\*
endif
" Enable the mouse if available
if has('mouse')
  set mouse=a
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors, themes and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

set background=dark
let g:solarized_termcolors=16

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T " no toolbar
    set guioptions-=e
    set guitablabel=%M\ %t
    set guifont=DejaVu\ Sans\ Mono\ 10 " Set the font
endif

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
nmap <space> <leader>

" Map escape to kj for fast return to normal mode
inoremap kj <ESC>

" Fast saving
nmap <leader>ww :w!<cr>
" :W sudo saves the file
" (useful for handling the permission-denied error)
command W w !sudo tee % > /dev/null

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Faster way to manipulate windows (I can't deal with ^W start)
" Right/down/up/left
nmap <leader>wj <C-W>j
nmap <leader>wk <C-W>k
nmap <leader>wh <C-W>h
nmap <leader>wl <C-W>l
" Open splits
nmap <leader>wv :vsp<CR>
nmap <leader>ws :sp<CR>
" Close splits
nmap <leader>wc <C-W>c
nmap <leader>wo <C-W>o
" Move splits
nmap <leader>wJ <C-W>J
nmap <leader>wK <C-W>K
nmap <leader>wH <C-W>H
nmap <leader>wL <C-W>L
" Quick resizing of windows
nmap <leader>w= <C-W>=
nmap <leader>w- <C-W>_
nmap <leader>w/ <C-W>\|

" Move a line of text using Meta+[jk]
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z
if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

" Generate recursive downwards ctags in cur. dir
map <silent><leader>ct :exe ':silent !ctags -R -f ./tags . &'<cr>:redraw!<cr>

" Toggle tagbar
map <silent><leader>tb :TagbarToggle<cr>

" Delete trailing white space on save
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite * :call DeleteTrailingWS()

" Use tabs instead of spaces in makefiles
autocmd FileType make setlocal noexpandtab

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rainbow Parentheses configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound    " ()
au Syntax * RainbowParenthesesLoadSquare   " []
au Syntax * RainbowParenthesesLoadBraces   " {}
au Syntax * RainbowParenthesesLoadChevrons " <>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim-clang-format configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map to <Leader>cf in C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Airline configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#tabline#enabled = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ultisnips configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Triggers
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" let g:UltiSnipsListSnippets="<s-tab>"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ctrl-P configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Change the default binding and command name
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
" Change the starting directory search pattern
let g:ctrlp_working_path_mode = 'ra'
" Add additional default base directory markers e.g. '.git', '.svn' etc.
let g:ctrlp_root_markers = []
" What to do if file already open as buffer (here we switch window to buffer)
let g:ctrlp_switch_buffer = 'et'
" Files to ignore
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
" Leader binding for using ctrlp to search for tags in tag file
nnoremap <leader>. :CtrlPTag<cr>
