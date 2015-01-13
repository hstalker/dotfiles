" PLUGINS CONFIG
" buffergator
nmap <silent><Leader>b :EasyBuffer<CR>

" airline
let g:airline_enable_branch=1
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_inactive_collapse=1
"let g:bufferline_echo = 0
"let g:airline#extensions#bufferline#enabled = 1
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
        "let g:airline_left_sep = ''
        "let g:airline_right_sep = ''
        let g:airline_symbols.branch = ''
        let g:airline_symbols.linenr = ''
    endif
endif

" ctrlp
let g:ctrlp_cache_dir = '/home/.vim/.ctrlp_cache'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_extensions = ['funky']

nmap <silent>cp :CtrlPMixed<CR>
nmap <silent>cm :CtrlPMRUFiles<CR>
nmap <silent>cf :CtrlPFunky<CR>
nmap <silent>cl :CtrlPLine<CR>
nmap <silent>cb :CtrlPBuffer<CR>
nmap <silent>ct :CtrlPBufTag<CR>

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

let g:ctrlp_user_command = {
    \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
    \ 2: ['.hg', 'hg --cwd %s locate -i .'],
    \ },
    \ 'fallback': s:ctrlp_fallback
    \ }

" easymotion
let g:EasyMotion_leader_key = 'e'
hi link EasyMotionTarget ErrorMsg
hi link EasyMotionShade  Comment

" fugitive
nmap <silent> <leader>gs :Gstatus<CR>
nmap <silent> <leader>gd :Gdiff<CR>
nmap <silent> <leader>gc :Gcommit<CR>
nmap <silent> <leader>gb :Gblame<CR>
nmap <silent> <leader>gl :Glog<CR>
nmap <silent> <leader>gp :Git push<CR>
nmap <silent> <leader>gr :Gread<CR>
nmap <silent> <leader>gw :Gwrite<CR>
nmap <silent> <leader>ge :Gedit<CR>
" Mnemonic _i_nteractive
nmap <silent> <leader>gi :Git add -p %<CR>
nmap <silent> <leader>gg :SignifyToggle<CR>

" indent guides
let g:indentLine_char = '│'
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

" signify
let g:signify_sign_overwrite = 1
let g:signify_sign_add = '+'
let g:signify_sign_change = '!'
let g:signify_sign_delete = '-'
let g:signify_sign_delete_first_line = '-'

" syntastic
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_jump=0
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=5
let g:syntastic_enable_signs=1
let g:syntastic_error_symbol='✗'
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
" Disable NeoComplCache for certain filetypes
autocmd FileType c,cpp,python,haskell NeoCompleteLock
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_delimiter = 1
let g:neocomplete#max_list = 15
let g:neocomplete#force_overwrite_completefunc = 0

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.xml='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.html='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.markdown='<[^>]*'
let g:neocomplete#sources#omni#input_patterns.css='^\s\+\w+\|\w+[):;]?\s\+\|[@!]'
let g:neocomplete#sources#omni#input_patterns.less='^\s\+\w+\|\w+[):;]?\s\+\|[@!]'
let g:neocomplete#sources#omni#input_patterns.javascript='[^. \t]\.\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.json='[^. \t]\.\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.ruby='[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.php='[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.go='\h\w*\%.'
let g:neocomplete#sources#omni#input_patterns.perl='\h\w*->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.java='\%(\h\w*\|)\)\.'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" haskellmode-vim
let g:haddock_browser="chromium"
autocmd FileType haskell set cmdheight=1
" necoghc
autocmd FileType haskell let g:ycm_semantic_triggers = {'haskell' : ['re![_a-zA-Z]+[_\w]*\.', '.']}
"let g:ycm_cache_omnifunc = 1
let g:necoghc_enable_detailed_browse = 1
let $PATH = $PATH . ':' . expand("~/.cabal/bin")
" Remap the ctrl-x and ctrl-o autocompletion shortcuts to something easier
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" YouCompleteMe
let g:ycm_auto_trigger = 1
let g:ycm_min_num_of_chars_for_completion = 3
"set completeopt=menu
let g:ycm_add_preview_to_completeopt = 0


" DelimitMate
let delimitMate_matchpairs = "(:),[:],{:}"

" Ultisnips
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

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-e>"
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
  