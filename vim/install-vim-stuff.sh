#!/bin/sh

mkdir -p /home/.vim
sudo cp -rf settings/bundles.vim /home/.vim/bundles.vim
sudo cp -rf settings/settings.vim  /home/.vim/settings.vim
sudo cp -rf settings/map.vim  /home/.vim/map.vim
sudo cp -rf settings/plugins.vim  /home/.vim/plugins.vim
sudo cp -rf settings/functions.vim  /home/.vim/functions.vim
sudo cp -rf settings/autocmd.vim  /home/.vim/autocmd.vim
sudo cp -rf settings/fonts  /home/.vim/fonts
sudo cp -rf settings/spell  /home/.vim/spell
sudo cp -rf vimrc  /etc/vimrc
# sudo cp -rf ./vimrc /etc/vim/vimrc
sudo chmod -R 777 /home/.vim

