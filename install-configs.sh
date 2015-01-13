#!/usr/bin/env bash

# Add the AUR repos if they aren't already added
if grep -Fxq $'[archlinuxfr]' /etc/pacman.conf
then
    echo 'AUR packages already added to database...'
else
    echo 'Adding AUR packages to database...'
    echo $'[archlinuxfr]\nSigLevel = Never\nServer = http://repo.archlinux.fr/$arch\n' | sudo tee -a /etc/pacman.conf
fi
# Grab packages
echo 'Grabbing pacmatic...'
sudo pacman -Syu pacmatic
echo 'Grabbing dependencies for this installation...'
sudo pacmatic -S autoconf automake make gcc ghc cabal-install \
python2 python3 vim i3 rxvt-unicode xorg-xrdb xorg-xinit dmenu \
llvm clang boost cmake ttf-dejavu bash-completion astyle git \
conky yaourt feh acpi
yaourt -Sy clarity-icon-theme gtk-theme-flatstudio

# Copy stuff
echo 'Copying backgrounds...'
cp -rf backgrounds ~/
echo 'Copying configuration files...'
cp -rf .i3 ~/
sudo chmod u+x ~/.i3/conky-i3.sh
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
echo -n "Enter your working internet providing card's name: "
read internet_card
sed "s/enp0s3/$internet_card/g" .conkyrc > ~/.conkyrc


# Build stuff
echo 'Installing Vim plugins...'
vim +PluginInstall +qall
cd ~/.vim/bundle/vimproc.vim && make
cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer --system-libclang --system-boost

echo 'Installing extra haskell binaries into path...'
mkdir ~/.ghc
cd ~/.ghc
cabal sandbox init
cabal update
cabal install alex happy hlint ghc-mod-5.2.1.2
