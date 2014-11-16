#!/bin/sh

sudo pacmatic -Syu pacmatic autoconf automake make gcc ghc cabal-install python2 python3 vim nasm yasm i3 rxvt-unicode xorg-xrdb dmenu llvm clang boost
cp -rf .i3 ~/.i3
cp .vim/.vimrc ~/.vimrc
cp -rf .vim/settings ~/.vim/settings
cp .bashrc ~/.bashrc
cp .xinitrc ~/.xinitrc
cp .Xresources ~/.Xresources
cp .ycm_extra_conf.py ~/.ycm_extra_conf.py
vim +PluginInstall +qall
cd ~/.vim/bundle/vimproc.vim && make
cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer --system-libclang
