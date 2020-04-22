" We choose to use a dumb ag/rg based jump as opposed to tags
let g:any_jump_disable_default_keybindings = 1
let g:any_jump_list_numbers = 1
let g:any_jump_references_enabled = 1
let g:any_jump_grouping_enabled = 1
let g:any_jump_preview_lines_count = 5
let g:any_jump_max_search_results = 10
let g:any_jump_search_prefered_engine = 'rg'
let g:any_jump_results_ui_style = 'filename_first'
let g:any_jump_window_width_ratio = 0.3
let g:any_jump_window_height_ratio = 0.4
let g:any_jump_window_top_offset = 4
let g:any_jump_colors = {
  \ "plain_text": "Comment",
  \ "preview": "Comment",
  \ "preview_keyword": "Operator",
  \ "heading_text": "Function",
  \ "heading_keyword": "Identifier",
  \ "group_text": "Comment",
  \ "group_name": "Function",
  \ "more_button": "Operator",
  \ "more_explain": "Comment",
  \ "result_line_number": "Comment",
  \ "result_text": "Statement",
  \ "result_path": "String",
  \ "help": "Comment"
  \ }
let g:any_jump_remove_comments_from_results = 1
let g:any_jump_ignored_files = ['*.tmp', '*.temp']
let g:any_jump_references_only_for_current_filetype = 0
let g:any_jump_disable_vcs_ignore = 0

let g:which_key_map.j.c = [':AnyJump', 'any-jump-under-cursor']
let g:which_key_map.j.s = [':AnyJumpVisual', 'any-jump-selection']
let g:which_key_map.j.p = [':AnyJumpBack', 'any-jump-previous']
let g:which_key_map.j.l = [':AnyJumpLastResults', 'any-jump-last-results']

