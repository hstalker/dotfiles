" Use tags inside git directory
set tags^=.git/tags;~
nnoremap <C-]> g<C-]>
vnoremap <C-]> g<C-]>
let g:which_key_map.j.d = ['<C-]>', 'tag-push-down-and-jump']
let g:which_key_map.j.u = [':pop', 'tag-pop-up-and-jump']
let g:which_key_map.j.t = {'name': '+tags-additional'}
let g:which_key_map.j.t.p = [':tprevious', 'tag-previous-match']
let g:which_key_map.j.t.n = [':tnext', 'tag-next-match']
let g:which_key_map.j.t.l = [':tags', 'tag-list-stack']

" Tags bindings
let g:which_key_map.p.t = [':CtrlPTag', 'fuzzy-tag-search']

" Tagbar setup
" Defaults to space which conflicts with which-key (not useful anyway)
let g:tagbar_map_showproto = "P"
let g:which_key_map.t.t = [':TagbarToggle', 'toggle-tagbar']

