" PLUGINS BUNDLE
if empty(glob('~/.vim/bundle/neobundle.vim/README.md'))
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim/
endif

if has('vim_starting')
    set nocompatible                                    " Be iMproved
    set runtimepath+=~/.vim/bundle/neobundle.vim/       " NeoBundle, required
endif

call neobundle#rc(expand('~/.vim/bundle/'))             " NeoBundle, required
NeoBundleFetch 'Shougo/neobundle.vim'                   " NeoBundle, required

filetype plugin indent on                               " NeoBundle, required
NeoBundleCheck                                          " NeoBundle, required

" Plugin Groups
" List only the plugin groups you will use
if !exists('g:bundle_groups')
    let g:bundle_groups=['general', 'devel', 'languages', 'colorscheme']
endif

" Plugins here:
" GENERAL
if count(g:bundle_groups, 'general')
    NeoBundle 'Stormherz/tablify'
    NeoBundle 'scrooloose/nerdtree'
    NeoBundle 'chrisbra/NrrwRgn'
    NeoBundle 'kien/ctrlp.vim'
    NeoBundle 'tacahiroy/ctrlp-funky'
    NeoBundle 'kris89/vim-multiple-cursors'
    NeoBundle 'mbbill/undotree'
    NeoBundle 'mhinz/vim-startify'
    NeoBundle 'tpope/vim-abolish'
    NeoBundle 'tpope/vim-repeat'
    NeoBundle 'tpope/vim-surround'
    NeoBundle 'tpope/vim-vinegar'
    NeoBundle 'troydm/easybuffer.vim'
    NeoBundle 'yonchu/accelerated-smooth-scroll'
    NeoBundle 'nvie/vim-togglemouse'
    NeoBundle 'hwrod/interactive-replace'
    NeoBundle 'bling/vim-airline'
endif
" DEVELOPER
if count(g:bundle_groups, 'devel')
    NeoBundle 'Shougo/neocomplete'
    NeoBundle 'SirVer/ultisnips'
    NeoBundle 'honza/vim-snippets'
    NeoBundle 'AzizLight/TaskList.vim'
    NeoBundle 'Raimondi/delimitMate'
    NeoBundle 'Yggdroot/indentLine'
    NeoBundle 'godlygeek/tabular'
    NeoBundle 'jbnicolai/rainbow_parentheses.vim'
    NeoBundle 'scrooloose/nerdcommenter'
    NeoBundle 'scrooloose/syntastic'
    NeoBundle 'majutsushi/tagbar'
endif
" LANGUAGES
if count(g:bundle_groups, 'languages')
    NeoBundleLazy 'davidhalter/jedi-vim', {'autoload' : {'filetypes' : ['python'] } }
    NeoBundleLazy 'lukerandall/haskellmode-vim', {'autoload' : {'filetypes' : ['haskell'] } }
endif
" COLORSCHEME
if count(g:bundle_groups, 'colorscheme')
    NeoBundle 'tomasr/molokai'
endif

NeoBundleCheck

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Do settings configurations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" buffergator
nmap <silent><Leader>b :EasyBuffer<CR>

" airline
let g:airline_enable_branch=1
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_inactive_collapse=1
let g:bufferline_echo = 0
let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#hunks#enabled = 1
let g:airline#extensions#ctrlp#show_adjacent_modes = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='molokai'
let g:airline_left_sep = ''
let g:airline_right_sep = ''
if GUI()
    if !exists('g:airline_symbols')
        let g:airline_symbols = {}
    endif
    if !exists('g:airline_powerline_fonts')
        "let g:airline_left_sep = 'рс'
        "let g:airline_right_sep = 'ру'
        let g:airline_symbols.branch = 'рб'
        let g:airline_symbols.linenr = 'рв'
    endif
endif

" ctrlp
let g:ctrlp_cache_dir = '~/.vim/.ctrlp_cache'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_extensions = ['funky']

let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|\.svn$',
    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

" On Windows use "dir" as fallback command.
if WINDOWS()
    let s:ctrlp_fallback = 'dir %s /-n /b /s /a-d'
elseif executable('ag')
    let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
elseif executable('ack')
    let s:ctrlp_fallback = 'ack %s --nocolor -f'
else
    let s:ctrlp_fallback = 'find %s -type f'
endif

" indent guides
let g:indentLine_char = '„ '
let g:indentLine_faster = 1

" NERDCommenter
nmap ; <Plug>NERDCommenterToggle
vmap ; <Plug>NERDCommenterToggle

" NERDTree
nmap <silent><Leader>nt :NERDTreeToggle<CR>
let g:NERDTreeBookmarksFile = '/home/.vim/.NERDTreeBookmarks'
let g:NERDTreeWinPos = "right"
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeWinSize = 40
let g:NERDTreeChristmasTree = 0
let g:NERDTreeCaseSensitiveSort = 1
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeShowHidden = 1
let g:NERDTreeMouseMode = 2
let g:NERDTreeIgnore=[
    \'\.pyc$', '\.pyo$', '\.py\$class$', '\.obj$',
    \ '\.o$', '\.so$', '\.egg$', '^\.git$', '^\.svn$' ]

" less
nmap <Leader>css :w <BAR> !lessc % > %:t:r.css<CR><space>

" rainbow parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" syntastic
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_jump=0
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=5
let g:syntastic_enable_signs=1
let g:syntastic_error_symbol='?'
let g:syntastic_warning_symbol='?'

" tabularize
vmap <Leader>a=  :Tabularize /=<CR>
vmap <Leader>a#  :Tabularize /#<CR>
vmap <Leader>a'  :Tabularize /'<CR>
vmap <Leader>a"  :Tabularize /"<CR>
vmap <Leader>a)  :Tabularize /)/r1c1l0<CR>
vmap <Leader>a== :Tabularize /=/r1c1l0<CR>
vmap <Leader>a:  :Tabularize /:<CR>
vmap <Leader>a:: :Tabularize /:\zs<CR>
vmap <Leader>a,  :Tabularize /,<CR>
vmap <Leader>a,, :Tabularize /,\zs<CR>

" undotree
nmap <silent>U :UndotreeToggle<CR>
" If undotree is opened, it is likely one wants to interact with it.
let g:undotree_SetFocusWhenToggle=1

" neocomplete
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_delimiter = 1
let g:neocomplete#max_list = 15

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.xml='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.html='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.markdown='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.json='[^. \t]\.\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.php='[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.java='\%(\h\w*\|)\)\.'

" DelimitMate
let delimitMate_matchpairs = "(:),[:],{:},<:>"

" Ultisnips
let g:UltiSnipsExpandTrigger       = "<tab>"
let g:UltiSnipsJumpForwardTrigger  = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
"let g:UltiSnipsSnippetDirectories  = ["snips"]

function! g:UltiSnips_Complete()
    call UltiSnips#ExpandSnippet()
    if g:ulti_expand_res == 0
        if pumvisible()
            return "\<C-n>"
        else
            call UltiSnips#JumpForwards()
            if g:ulti_jump_forwards_res == 0
               return "\<TAB>"
            endif
        endif
    endif
    return ""
endfunction

au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"

" Jedi
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_select_first = 0
