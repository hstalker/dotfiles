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

source ~/.vim/settings/bundles.vim
source ~/.vim/settings/settings.vim
source ~/.vim/settings/map.vim
source ~/.vim/settings/plugins.vim
source ~/.vim/settings/functions.vim
source ~/.vim/settings/autocmd.vim
