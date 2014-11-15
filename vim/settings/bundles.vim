" PLUGINS BUNDLE
let vundle_readme='/home/.vim/bundle/vundle/README.md'
if !filereadable(vundle_readme)
  echo "Installing Plugin.."
  echo ""
  silent !mkdir -p /home/.vim/bundle
  silent !git clone https://github.com/gmarik/vundle /home/.vim/bundle/vundle/
endif

" Required:
if has('vim_starting')
  set nocompatible
  set runtimepath+=/home/.vim/bundle/vundle/
  set sessionoptions-=options
endif

call vundle#rc('/home/.vim/bundle/')
Plugin 'gmarik/vundle'

" Plugin Groups
" List only the plugin groups you will use
if !exists('g:bundle_groups')
  let g:bundle_groups=['general', 'devel', 'languages', 'colorscheme']
endif

" Plugins here:
" GENERAL
if count(g:bundle_groups, 'general')
  Plugin 'Lokaltog/vim-easymotion'
  Plugin 'Stormherz/tablify'
  Plugin 'scrooloose/nerdtree'
  Plugin 'chrisbra/NrrwRgn'
  Plugin 'kien/ctrlp.vim'
  Plugin 'tacahiroy/ctrlp-funky'
  Plugin 'kris89/vim-multiple-cursors'
  Plugin 'mbbill/undotree'
  Plugin 'mhinz/vim-startify'
  Plugin 'tpope/vim-abolish'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-fugitive'
  Plugin 'tpope/vim-repeat'
  Plugin 'tpope/vim-surround'
  Plugin 'tpope/vim-vinegar'
  Plugin 'tpope/vim-bundler'
  Plugin 'troydm/easybuffer.vim'
  Plugin 'yonchu/accelerated-smooth-scroll'
  Plugin 'nvie/vim-togglemouse'
  "replace tools
  Plugin 'dkprice/vim-easygrep'
  Plugin 'hwrod/interactive-replace'
  Plugin 'bling/vim-airline'
endif
" DEVELOPER
if count(g:bundle_groups, 'devel')
  Plugin 'Shougo/neocomplete'
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'
  Plugin 'Shougo/vimproc.vim'
  Plugin 'AzizLight/TaskList.vim'
  Plugin 'Chiel92/vim-autoformat'
  Plugin 'Raimondi/delimitMate'
  Plugin 'Yggdroot/indentLine'
  Plugin 'gcmt/wildfire.vim'
  Plugin 'godlygeek/tabular'
  Plugin 'jbnicolai/rainbow_parentheses.vim'
  Plugin 'mhinz/vim-signify'
  Plugin 'scrooloose/nerdcommenter'
  Plugin 'scrooloose/syntastic'
  Plugin 'majutsushi/tagbar'
endif
" LANGUAGES
if count(g:bundle_groups, 'languages')
  Plugin 'pangloss/vim-javascript'
  Plugin 'othree/javascript-libraries-syntax.vim'
  Plugin 'ap/vim-css-color'
  Plugin 'burnettk/vim-angular'
  Plugin 'davidhalter/jedi-vim'
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'tikhomirov/vim-glsl'
  Plugin 'wting/rust.vim'
  Plugin 'lukerandall/haskellmode-vim'
  Plugin 'eagletmt/neco-ghc'
  Plugin 'Valloric/YouCompleteMe'
endif
" COLORSCHEME
if count(g:bundle_groups, 'colorscheme')
  Plugin 'tomasr/molokai'
  Plugin 'jeffreyiacono/vim-colors-wombat'
  Plugin 'altercation/vim-colors-solarized'
  Plugin 'morhetz/gruvbox'
  Plugin 'sjl/badwolf'
  Plugin 'chriskempson/base16-vim'
endif

" automatically load filetype plugins
filetype plugin indent on
