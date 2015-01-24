" AUTOCOMMANDS
if has("autocmd")
    augroup filetypedetect
    au BufEnter Makefile setl ts=4 sts=4 sw=4 noet list
    augroup END

    " when enabling diff for a buffer it should be disabled when the
    " buffer is not visible anymore
    au BufHidden * if &diff == 1 | diffoff | setlocal nowrap | endif

    " Instead of reverting the cursor to the last position in the buffer, we
    " set it to the first line when editing a git commit message
    au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

    " Automatically removing all trailing whitespace
    autocmd BufWritePre * :call StripTrailingWhitespace()

    " Resize splits when the window is resized
    au VimResized * exe "normal! \<c-w>="
endif
