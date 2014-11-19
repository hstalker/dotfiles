#!/bin/sh

sudo pacmatic -Syu pacmatic autoconf automake make gcc ghc cabal-install \
python2 python3 vim nasm yasm i3 rxvt-unicode xorg-xrdb xorg-xinit dmenu \
llvm clang boost cmake ttf-dejavu
cp -rf .i3 ~/
cp .vimrc ~/.vimrc
cp -rf .vim ~/
cp .bashrc ~/.bashrc
cp .bash_profile ~/.bash_profile
cp .xinitrc ~/.xinitrc
cp .Xresources ~/.Xresources
cp .ycm_extra_conf.py ~/.ycm_extra_conf.py
vim +PluginInstall +qall
cd ~/.vim/bundle/vimproc.vim && make
cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer --system-libclang
