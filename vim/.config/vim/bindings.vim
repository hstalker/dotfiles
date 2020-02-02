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

