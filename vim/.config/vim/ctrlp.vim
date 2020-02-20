let g:ctrlp_map = ''
let g:which_key_map.p.f = [':CtrlP', 'fuzzy-file-search']
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

