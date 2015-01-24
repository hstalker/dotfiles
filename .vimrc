" Identify platform
silent function! OSX()
  return has('macunix')
endfunction
silent function! LINUX()
  return has('unix') && !has('macunix') && !has('win32unix')
endfunction
silent function! WINDOWS()
  return (has('win16') || has('win32') || has('win64'))
endfunction
silent function! GUI()
  return (has('gui_running'))
endfunction

" Multiplatform compatibility
if WINDOWS()
  " On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
  " across (heterogeneous) systems easier.
  set rtp=~/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,~/.vim/after
endif

if !empty(glob('~/.vim/bundles.vim'))
    source ~/.vim/bundles.vim
endif
if !empty(glob('~/.vim/settings.vim'))
    source ~/.vim/settings.vim
endif
if !empty(glob('~/.vim/map.vim'))
    source ~/.vim/map.vim
endif
if !empty(glob('~/.vim/functions.vim'))
    source ~/.vim/functions.vim
endif
if !empty(glob('~/.vim/autocmd.vim'))
    source ~/.vim/autocmd.vim
endif
