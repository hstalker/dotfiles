" Change leader key to space
let g:mapleader = "\<Space>"
" For filetype specific leader bindings
let g:localleader = ','

" Map escape to kj for fast return to normal mode
inoremap kj <ESC>


" Spacemacs-esque bindings via whichkey
" Setup which-key
let g:which_key_map = {}
call which_key#register(g:mapleader, 'g:which_key_map')
nnoremap <silent> <leader> :<C-u>WhichKey '<leader>'<CR>
nnoremap <silent> <localleader> :<C-u>WhichKey ','<CR>
vnoremap <silent> <leader> :<C-u>WhichKeyVisual '<leader>'<CR>
let g:which_key_vertical = 0 " Should guide appear vertically?
let g:which_key_position = 'botright' " Guide split position
let g:which_key_hspace = 2 " Min horizontal space between columns

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

function! g:ToggleSyntaxHighlighting()
  if exists("g:syntax_on")
    syntax off
  else
    syntax enable
  endif
endfunction
let g:which_key_map.t = { 'name': '+toggles' }
let g:which_key_map.t.s = { 'name': '+syntax' }
let g:which_key_map.t.s.h = [':call g:ToggleSyntaxHighlighting()', 'syntax-hl']

function! g:RenameCurrentBuffer()
  let l:new_name = input("New buffer name: ")
  execute ":f " . l:new_name
endfunction
function! g:FindFileLiterally()
  let l:path = input("Literal file path: ")
  execute ":e " . l:path
endfunction
let g:which_key_map.f = { 'name': '+files' }
let g:which_key_map.f.d = [":call delete(expand('%'))", 'delete-current']
let g:which_key_map.f.v = { 'name': '+vim' }
let g:which_key_map.f.v.d = [':e ' . g:vim_cfg_dir . '/vimrc', 'find-dotfile']
let g:which_key_map.f.v.r = [':source ' . g:vim_cfg_dir . '/vimrc', 'source-dotfile']
let g:which_key_map.f.C = { 'name': '+conversion' }
let g:which_key_map.f.C.d = [':set fileformat=dos', 'unix2dos']
let g:which_key_map.f.C.u = [':set fileformat=unix', 'dos2unix']
let g:which_key_map.f.w = [':w', 'write-buffer']
let g:which_key_map.f.W = [':wa', 'write-all']
let g:which_key_map.f.r = [
  \ ":call delete(expand('%')) \| call g:RenameCurrentBuffer()",
  \ 'rename'
  \ ]
let g:which_key_map.f.y = [':let @" = expand("%")', 'yank-file-name']
let g:which_key_map.f.l = [':call FindFileLiterally()', 'find-literally']
let g:which_key_map.f.S = [':w !sudo tee %', 'sudo-write-buffer']

let g:which_key_map.h = [':h', 'help']

let g:which_key_map.e = { 'name': '+errors' }
let g:which_key_map.e.o = [':copen', 'open']
let g:which_key_map.e.h = [':cclose', 'hide']
let g:which_key_map.e.c = [':cc', 'display-current']
let g:which_key_map.e.n = [':cnext', 'next']
let g:which_key_map.e.p = [':cprev', 'previous']

let g:which_key_map.j = { 'name': '+jumps' }
let g:which_key_map.j.l = [':jumps', 'jumplist-list']
let g:which_key_map.j.b = ['<C-o>', 'jumplist-backward']
let g:which_key_map.j.f = ['<C-i>', 'jumplist-forward']

let g:which_key_map.c = { 'name': '+compilation' }
let g:which_key_map.c.m = [':make', 'make']

let g:which_key_map.p = { 'name': '+projects' }

