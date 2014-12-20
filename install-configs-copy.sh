#!/bin/sh

# Copy stuff
cp -rf .i3 ~/
cp .vimrc ~/.vimrc
cp -rf .vim ~/
cp .bashrc ~/.bashrc
cp .bash_profile ~/.bash_profile
cp .xinitrc ~/.xinitrc
cp .Xresources ~/.Xresources
cp -rf .config ~/
cp .gtkrc-2.0 ~/.gtkrc-2.0
cp .gitconfig ~/.gitconfig
cp .ycm_extra_conf.py ~/.ycm_extra_conf.py
cp .astylerc ~/.astylerc
cp -rf .opp ~/
cp .zshrc ~/.zshrc
cp .zcompdump ~/.zcompdump


# Build stuff
vim +PluginInstall +qall
cd ~/.vim/bundle/vimproc.vim && make
cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer --system-libclang --system-boost
