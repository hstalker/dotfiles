let g:NERDTreeShowHidden = 1
" Customize arrows
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
" Exit vim if only thing open is nerdtree
autocmd bufenter *
  \ if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree())
  \   | q | 
  \ endif

let g:which_key_map.t.n = [':NERDTreeToggle', 'toggle-nerdtree']

