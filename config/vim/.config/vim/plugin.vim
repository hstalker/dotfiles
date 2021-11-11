scriptencoding utf-8

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use VimPlug for plugin management
let g:plug_dir=g:data_dir . 'plug/'
let g:plug_script=g:plug_dir . 'plug.vim'
" We fix the hash for reproducibility, so this will need to be manually
" updated.
let g:plug_commit_hash='c997134'
let g:plug_url='https://raw.githubusercontent.com/junegunn/vim-plug/'
  \ . g:plug_commit_hash . '/plug.vim'
" Grab vimplug if not available using curl
if empty(glob(g:plug_script))
  execute 'silent !curl -fLo ' . g:plug_script . ' --create-dirs ' . g:plug_url
endif
execute 'source ' . g:plug_script

" Customize built-in plug parameters
let g:plug_threads = 8
let g:plug_timeout = 60
let g:plug_retries = 2 " No. times to retry failed install
let g:plug_shallow = 1 " Shallow clone
let g:plug_window = 'vertical topleft new' " Plug window location
let g:plug_pwindow = 'above 12new' " Plug diff window location
let g:plug_url_format = 'https://git::@github.com/%s.git'


" Automatically install plugins that haven't been installed already
augroup PLUG
  autocmd! PLUG
  autocmd VimEnter *
    \ if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \   |   PlugInstall --sync | q
    \   | endif
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Default plugins and pinnings
" Theming (solarized8 supports truecolor, whereas old vim-solarized does not)
call core#PluginAdd('lifepillar/vim-solarized8')
" Status line prettification
call core#PluginAdd('vim-airline/vim-airline')
call core#PluginAdd('vim-airline/vim-airline-themes')
" Spacemacs (which-key) style dynamic keybinding helper menu
call core#PluginAdd('liuchengxu/vim-which-key')
" Make the `.` command work for more complex actions
call core#PluginAdd('tpope/vim-repeat')
" Primitive motions for manipulating surrounded text (''/""/() etc.)
call core#PluginAdd('tpope/vim-surround')
" Commenting blocks
call core#PluginAdd('tpope/vim-commentary')
" Autosave documents on hooks
call core#PluginAdd('907th/vim-auto-save')
" VCS LHS change display (e.g. additions/changes/deletions)
if has('nvim') || has('patch-8.0.902')
  call core#PluginAdd('mhinz/vim-signify')
endif
" Snippets
if (has('python') || has('python3'))
  call core#PluginAdd('SirVer/ultisnips')
  call core#PluginAdd('honza/vim-snippets')
endif
" Fuzzy buffer/file searching within project
call core#PluginAdd('ctrlpvim/ctrlp.vim')
" Filesytem tree explorer
call core#PluginAdd('preservim/nerdtree')
" 'Dumb' jumping for 40+ languages (as opposed to tags)
call core#PluginAdd('pechorin/any-jump.vim')
" Load editorconfig settings from file in directory
call core#PluginAdd('editorconfig/editorconfig-vim')
" Dynamically display effects of ex commands (e.g. :s etc.)
if v:version >= 801
  " Documentation of plugin suggests it only works on 8.1 or above, but this
  " doesn't appear to be true in practice
  call core#PluginAdd('markonm/traces.vim')
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Start up vim-plug, so that lock files can work their magic
call plug#begin(g:plug_dir)


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Load core plugin version lockfile - automanaged via vim-plug's snapshotting
call core#TrySource(g:config_dir . 'lock.vim')


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Load local custom plugin handling overrides
call core#TrySource(g:config_dir . 'custom.plugin.vim')


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Load plugin version lockfile overrides - automanaged via vim-plug's
" snapshotting
call core#TrySource(g:config_dir . 'custom.lock.vim')


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Run through final registry and register the information with vim-plug
call core#PluginProcessRegistry()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Finalize plugins
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Whichkey configuration - Spacemacs-esque bindings via whichkey
if core#PluginIsLoaded('liuchengxu/vim-which-key')
  " Setup which-key
  let g:which_key_map = {}
  let g:which_key_major_mode_map = {}
  call which_key#register(g:mapleader, 'g:which_key_map')
  call which_key#register(g:maplocalleader, 'g:which_key_major_mode_map')
  nnoremap <silent> <leader> :<C-u>WhichKey '<leader>'<CR>
  nnoremap <silent> <localleader> :<C-u>WhichKey '<localleader>'<CR>
  vnoremap <silent> <leader> :<C-u>WhichKeyVisual '<leader>'<CR>
  vnoremap <silent> <localleader> :<C-u>WhichKeyVisual '<localleader>'<CR>
  let g:which_key_vertical = 0 " Should guide appear vertically?
  let g:which_key_position = 'botright' " Guide split position
  let g:which_key_hspace = 2 " Min horizontal space between columns

  " Setup core which-key bindings
  let g:which_key_map.m = [g:maplocalleader, '+major-mode']

  let g:which_key_map.q = { 'name': '+quit' }
  let g:which_key_map.q.s = [':wqa', 'save-kill-vim']
  let g:which_key_map.q.q = [':conf q', 'prompt-kill-vim']
  let g:which_key_map.q.Q = [':qa!', 'kill-vim']


  let g:which_key_map.w = { 'name': '+windows' }
  for s:i in range(1, 9)
    execute "let g:which_key_map.w.".s:i."=['".s:i."<C-W><C-W>', 'jump-".s:i."']"
  endfor
  " Right/down/up/left
  let g:which_key_map.w.j = ['<C-W>j', 'select-down']
  let g:which_key_map.w.k = ['<C-W>k', 'select-up']
  let g:which_key_map.w.h = ['<C-W>h', 'select-left']
  let g:which_key_map.w.l = ['<C-W>l', 'select-right']
  " Alternate
  let g:which_key_map.w['<Tab>'] = ['<C-W>w', 'move-alternate']
  " Open splits
  let g:which_key_map.w.v = [':vsp', 'split-vertical']
  let g:which_key_map.w.s = [':sp', 'split-horizonital']
  " Close splits
  let g:which_key_map.w.d = ['<C-W>c', 'delete']
  let g:which_key_map.w.D = ['<C-W>o', 'delete-other']
  " Move splits
  let g:which_key_map.w.J = ['<C-W>J', 'move-down']
  let g:which_key_map.w.K = ['<C-W>K', 'move-up']
  let g:which_key_map.w.H = ['<C-W>H', 'move-left']
  let g:which_key_map.w.L = ['<C-W>L', 'move-right']
  let g:which_key_map.w.r = ['<C-W>r', 'rotate']
  " Resize splits
  let g:which_key_map.w['{'] = ['<C-W>+', 'increase-height']
  let g:which_key_map.w['}'] = ['<C-W>-', 'decrease-height']
  let g:which_key_map.w['['] = ['<C-W><', 'decrease-width']
  let g:which_key_map.w[']'] = ['<C-W>>', 'increase-width']
  let g:which_key_map.w['='] = ['<C-W>=', 'balance-size']
  let g:which_key_map.w['_'] = ['<C-W>_', 'maximize-size']


  let g:which_key_map.b = { 'name': '+buffers' }
  let g:which_key_map.b.l = [':buffers', 'list']
  let g:which_key_map.b.N = [':enew', 'new']
  let g:which_key_map.b.d = [':bd', 'delete']
  let g:which_key_map.b.D = [':%bd|e#', 'delete-others']
  let g:which_key_map.b.n = [':bn', 'next']
  let g:which_key_map.b.y = [':%y', 'yank']
  let g:which_key_map.b.p = [':bp', 'previous']
  for s:i in range(1, 9)
    execute "let g:which_key_map.b.".s:i."=[':b".s:i."', 'jump-".s:i."']"
  endfor

  let g:which_key_map.t = { 'name': '+toggles' }
  let g:which_key_map.t.s = { 'name': '+syntax' }
  let g:which_key_map.t.s.h = [':call core#SyntaxHlToggle()', 'syntax-hl']
  let g:which_key_map.t.t = [':call core#ThemeToggle()', 'theme']

  function! plugin#DeleteCurrentFile()
    call delete(expand("%"))
  endfunction

  function! plugin#RenameCurrentBuffer()
    let l:old_name = expand("%")
    let l:new_name = input("New buffer name: ", l:old_name, "file")
    call plugin#DeleteCurrentFile()
    execute ":f " . l:new_name
  endfunction

  function! plugin#FindFileLiterally()
    let l:path = input("Literal file path: ", "", "file")
    execute ":e " . l:path
  endfunction

  function! plugin#YankFileName()
    let @" = expand("%:p")
  endfunction

  function! plugin#YankFilePath()
    let @" = expand("%")
  endfunction

  let g:which_key_map.f = { 'name': '+files' }
  let g:which_key_map.f.d = [":call plugin#DeleteCurrentFile()", 'delete-current']
  let g:which_key_map.f.t = [":call core#TrimTrailingWhitespace()",
    \ 'trim-trailing-whitespace']
  let g:which_key_map.f.v = { 'name': '+vim' }
  let g:which_key_map.f.v.d = [':e ' . g:config_dir, 'find-dotfile']
  let g:which_key_map.f.v.r = [':source ' . g:config_dir . '/core.vim',
    \ 'source-dotfile']
  let g:which_key_map.f.C = { 'name': '+conversion' }
  let g:which_key_map.f.C.d = [':set fileformat=dos', 'unix2dos']
  let g:which_key_map.f.C.u = [':set fileformat=unix', 'dos2unix']
  let g:which_key_map.f.w = [':w', 'write-buffer']
  let g:which_key_map.f.W = [':wa', 'write-all']
  let g:which_key_map.f.r = [":call plugin#RenameCurrentBuffer()",
    \ 'rename-current']
  let g:which_key_map.f.y = [':call plugin#YankFileName', 'yank-file-name']
  let g:which_key_map.f.Y = [':call plugin#YankFilePath', 'yank-file-path']
  let g:which_key_map.f.l = [':call plugin#FindFileLiterally()',
    \ 'find-literally']
  let g:which_key_map.f.S = [':w !sudo tee %', 'sudo-write-buffer']

  let g:which_key_map.h = [':h', 'help']

  let g:which_key_map.e = { 'name': '+errors' }
  let g:which_key_map.e.o = [':copen', 'open']
  let g:which_key_map.e.h = [':cclose', 'hide']
  let g:which_key_map.e.c = [':cc', 'display-current']
  let g:which_key_map.e.n = [':cnext', 'next']
  let g:which_key_map.e.p = [':cprev', 'previous']

  let g:which_key_map.j = { 'name': '+jumps' }
  let g:which_key_map.j.j = [':jumps', 'jumplist-jumps']
  let g:which_key_map.j.b = ['<C-o>', 'jumplist-backward']
  let g:which_key_map.j.f = ['<C-i>', 'jumplist-forward']

  let g:which_key_map.c = { 'name': '+compilation' }
  let g:which_key_map.c.m = [':make', 'make']

  let g:which_key_map.p = { 'name': '+projects' }
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Airline configuration
if core#PluginIsLoaded('vim-airline/vim-airline')
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'
  let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
  if core#PluginIsLoaded('vim-airline/vim-airline-themes')
    let g:airline_theme='solarized'
  endif
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto-save configuration
if core#PluginIsLoaded('907th/vim-auto-save')
  let g:auto_save = 1 " Enable on startup
  let g:auto_save_silent = 0 " Display info on statusline
  let g:auto_save_write_all_buffers = 0 " Only save current buffer

  " Events upon which to save
  " Other useful events: TextChangedI, CursorHold, CursorHoldI, CompleteDone
  let g:auto_save_events = [
    \ "InsertLeave",
    \ "TextChanged",
    \ "FocusLost"
    \ ]

  " Pre/Post-save hooks
  let g:auto_save_presave_hook = ''
  let g:auto_save_postsave_hook = ''
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Anyjump configuration
if core#PluginIsLoaded('pechorin/any-jump.vim')
  " We choose to use a dumb ag/rg based jump as opposed to tags
  let g:any_jump_disable_default_keybindings = 1
  let g:any_jump_list_numbers = 1
  let g:any_jump_references_enabled = 1
  let g:any_jump_grouping_enabled = 1
  let g:any_jump_preview_lines_count = 5
  let g:any_jump_max_search_results = 10
  let g:any_jump_search_prefered_engine = 'rg'
  let g:any_jump_results_ui_style = 'filename_first'
  let g:any_jump_window_width_ratio = 0.3
  let g:any_jump_window_height_ratio = 0.4
  let g:any_jump_window_top_offset = 4
  let g:any_jump_colors = {
    \ "plain_text": "Comment",
    \ "preview": "Comment",
    \ "preview_keyword": "Operator",
    \ "heading_text": "Function",
    \ "heading_keyword": "Identifier",
    \ "group_text": "Comment",
    \ "group_name": "Function",
    \ "more_button": "Operator",
    \ "more_explain": "Comment",
    \ "result_line_number": "Comment",
    \ "result_text": "Statement",
    \ "result_path": "String",
    \ "help": "Comment"
    \ }
  let g:any_jump_remove_comments_from_results = 1
  let g:any_jump_ignored_files = ['*.tmp', '*.temp']
  let g:any_jump_references_only_for_current_filetype = 0
  let g:any_jump_disable_vcs_ignore = 0

  if core#PluginIsLoaded('liuchengxu/vim-which-key')
    let g:which_key_map.j.c = [':AnyJump', 'any-jump-under-cursor']
    let g:which_key_map.j.s = [':AnyJumpVisual', 'any-jump-selection']
    let g:which_key_map.j.p = [':AnyJumpBack', 'any-jump-previous']
    let g:which_key_map.j.l = [':AnyJumpLastResults', 'any-jump-last-results']
  endif
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colourscheme configuration
if core#PluginIsLoaded('lifepillar/vim-solarized8')
  let g:theme_dark='solarized8_flat'
  let g:theme_light='solarized8_flat'
  call core#ThemeUpdate()
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ultisnips configuration
if core#PluginIsLoaded('SirVer/ultisnips')
  let g:UltiSnipsSnippetDirectories = [
    \ g:config_dir . "UltiSnips",
    \ "UltiSnips"
    \ ]
  if core#PluginIsLoaded('honza/vim-snippets')
    call add(g:UltiSnipsSnippetDirectories, g:plug_dir . "vim-snippets/UltiSnips")
  endif
  let g:UltiSnipsExpandTrigger = "<tab>"
  let g:UltiSnipsJumpForwardTrigger = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nerdtree configuration
if core#PluginIsLoaded('preservim/nerdtree')
  let g:NERDTreeShowHidden = 1
  " Customize arrows
  let g:NERDTreeDirArrowExpandable = '▸'
  let g:NERDTreeDirArrowCollapsible = '▾'
  " Exit vim if only thing open is nerdtree
  augroup NERDTREE
    autocmd! NERDTREE
    autocmd bufenter *
      \ if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree())
      \   | q |
      \ endif
  augroup END

  if core#PluginIsLoaded('liuchengxu/vim-which-key')
    let g:which_key_map.t.n = [':NERDTreeToggle', 'nerdtree']
  endif
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CtrlP configuration
if core#PluginIsLoaded('ctrlpvim/ctrlp.vim')
  let g:ctrlp_map = ''
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
  let g:ctrlp_cache_dir = g:cache_dir . '/ctrlp'

  if core#PluginIsLoaded('liuchengxu/vim-which-key')
    let g:which_key_map.p.f = [':CtrlP', 'fuzzy-file-search']
  endif
endif

