" AUTOCOMMANDS
if has("autocmd")
    augroup filetypedetect
        au BufEnter Makefile setl ts=4 sts=4 sw=4 noet list
    augroup END

    " Automatically removing all trailing whitespace
    autocmd BufWritePre * :call StripTrailingWhitespace()

    " Resize splits when the window is resized
    au VimResized * exe "normal! \<c-w>="
endif
