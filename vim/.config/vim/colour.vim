" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T " no toolbar
  set guioptions-=e
  set guitablabel=%M\ %t
endif

" In case we haven't got the theme installed, just ignore errors
set background=light
try
  colorscheme solarized
catch
endtry

