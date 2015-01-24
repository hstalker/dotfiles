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
python2 python3 vim emacs i3 rxvt-unicode xorg-xrdb xorg-xinit dmenu \
llvm clang boost cmake ttf-dejavu bash-completion git \
conky yaourt feh acpi chromium
yaourt -Sy clarity-icon-theme gtk-theme-flatstudio

# Copy stuff
echo 'Copying backgrounds...'
echo 'Making scripts executable...'
sudo chmod u+x ./.i3/conky-i3.sh
echo -n "Enter your working internet providing card's name: "
read internet_card
sed "s/enp0s3/$internet_card/g" .conkyrc > ./.conkyrc


# Build stuff
echo 'Installing Vim plugins...'
vim +PluginInstall +qall

echo 'Installing extra haskell binaries into path...'
mkdir ./.ghc
cd ./.ghc
cabal sandbox init
cd ../
