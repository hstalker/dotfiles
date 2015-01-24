" FUNCTIONS
function! DualView()
  if &columns == '80'
    set lines=50 columns=160
    only
    vsplit
  else
    set lines=50 columns=80
    only
  endif
endfunction
nmap <silent><Leader>d :call DualView()<CR>

function! StripTrailingWhitespace()
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " do the business:
  %s/\s\+$//e
  " clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

" Enable the auto-creation of missing folders in a save path
if !exists('*s:MakeNewDir')
  function s:MakeNewDir(fullpath, buf)
    if empty(getbufvar(a:buf,'&buftype')) && a:fullpath!~#'\v^\w+\:\/'
      let dirpath=fnamemodify(a:fullpath,':h')
      if !isdirectory(dirpath)|call mkdir(dirpath,'p')|endif
    endif
  endfunction
  augroup WriteDir
    autocmd!
    autocmd BufWritePre * :call s:MakeNewDir(expand('<afile>'),+expand('<abuf>'))
  augroup END
endif
