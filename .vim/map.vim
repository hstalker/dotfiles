" KEYMAPPINGS
" Set mapleader
let g:mapleader="<Space>"

" ,/ turn off search highlighting
nmap <silent><Leader>/ :nohls<CR>

" Map escape key to jj or <Leader>e
imap <silent>kj <ESC>

" Quick alignment of text
nmap <Leader>al :left<CR>
nmap <Leader>ar :right<CR>
nmap <Leader>ac :center<CR>

" Improve up/down movement on wrapped lines
nnoremap j gj
nnoremap k gk

" Windows navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" move between buffers
nmap <C-S-TAB> :bprev<CR>
nmap <C-TAB> :bnext<CR>

" switch to the directory of the open buffer
map <Leader>cd :cd %:p:h<cr>

" Underline the current line with '-'
nmap <silent> <leader>ul :t.<CR>Vr-
