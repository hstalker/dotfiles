#! /usr/bin/sh

cp -rf .i3 /home/hector/.i3
sudo cp -rf .vim/settings/bundles.vim /home/.vim/bundles.vim
sudo cp -rf .vim/settings/settings.vim  /home/.vim/settings.vim
sudo cp -rf .vim/settings/map.vim  /home/.vim/map.vim
sudo cp -rf .vim/settings/plugins.vim  /home/.vim/plugins.vim
sudo cp -rf .vim/settings/functions.vim  /home/.vim/functions.vim
sudo cp -rf .vim/settings/autocmd.vim  /home/.vim/autocmd.vim
sudo cp -rf .vim/settings/fonts  /home/.vim/fonts
sudo cp -rf .vim/settings/spell  /home/.vim/spell
sudo cp -rf .vim/vimrc  /etc/vimrc
sudo chmod -R 777 /home/.vim
cp .bashrc ~/.bashrc
cp .xinitrc ~/.xinitrc
cp .ycm_extra_conf.py ~/.ycm_extra_conf.py

