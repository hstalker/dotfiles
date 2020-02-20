"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use VimPlug for plugin management
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
Plug 'ctrlpvim/ctrlp.vim', { 'on': 'CtrlP' }
if (has('python') || has('python3'))
  Plug 'SirVer/ultisnips', { 'on': ['UltiSnipsExpandTrigger'] }
  Plug 'honza/vim-snippets'
endif
Plug 'liuchengxu/vim-which-key'

call plug#end()
