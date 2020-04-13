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

