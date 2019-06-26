set nocompatible
filetype indent plugin on
syntax on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use VimPlug for plugin management
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vim_cache_dir=expand('~/.cache/vim/')
let g:vim_plugged_dir=g:vim_cache_dir . 'plugged/'
let g:vim_plug_script=g:vim_plugged_dir . 'plug.vim'
let g:vim_plug_url='https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
" Grab vimplug if not available using curl
if empty(glob(g:vim_plug_script))
  execute 'silent !curl -fLo ' . g:vim_plug_script . ' --create-dirs ' . g:vim_plug_url
  augroup PLUG
    autocmd! PLUG
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  augroup END
endif

exec 'source ' . g:vim_plug_script

" Call :PlugInstall to install, :PlugUpdate to update
call plug#begin(g:vim_plugged_dir)

Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'

call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let defaulttabsize = 2 " Sets all the tab options appropriately later on
set relativenumber " Set to use relative line mode
set number " Set to use absolute number for current line only
set textwidth=0 " Hard line wrap (number of cols)
set wrapmargin=0
set wrap
set linebreak " Break lines at word (requires Wrap lines)
" set columns=80 " Soft line wrap (number of cols)
set showbreak=+++ " Wrap-broken line prefix
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
set foldcolumn=1 " Add a bit extra margin to the left
set laststatus=2 " Always show the status line
set viminfo=""
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

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

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

" Grepping utils
if executable('ag')
  " Prefer ag
  let &grepprg = 'ag --smart-case --nogroup --nocolor --vimgrep'
  set grepformat^=%f:%l:%c:%m
endif
command! -nargs=+ GrepQF silent! grep <args> | cw | redraw!
nnoremap <leader>s :GrepQF<Space>
nnoremap <leader>sw :GrepQF <C-R><C-W><CR>

" Change netrwhist directory
let g:netrw_home=g:vim_cache_dir

" Leave a mark on the current line of the buffer when leaving based
" on the language of the file contents. Use capital letters only.
augroup VIMRC
  autocmd! VIMRC
  autocmd BufLeave *.C,*.H,*.c,*.h,*.cc,*.hh,*.c++,*.h++,*.cpp,*.hpp normal! mC
  autocmd BufLeave .vimrc,*.vim normal! mV
  autocmd BufLeave *.sh normal! mB
  autocmd BufLeave *.py normal! mP
  autocmd BufLeave *.hs normal! mH
augroup END

" Delete trailing white space on save
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
augroup TRAILINGWS
  autocmd! TRAILINGWS
  autocmd BufWrite * :call DeleteTrailingWS()
augroup END

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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin configuration
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrl-p
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
" Neither custom_ignore nor show_hidden work when we have user_command custom
" set
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|o|obj)$',
  \ 'link': '',
  \ }
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_use_caching = 1
if executable('ag')
  " Use ag for ctrlp search
  let g:ctrlp_user_command = 'ag %s '
    \ . '--ignore .git/\* --ignore .svn/\* --ignore .hg/\* '
    \ . '--hidden -l --smart-case --nocolor -g ""'
endif

" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_theme='solarized'

" Load a user-defined custom overrides file if it exists
if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

