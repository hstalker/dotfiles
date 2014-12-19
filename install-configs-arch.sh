#!/bin/sh

# Grab packages
sudo pacman -Syu pacmatic
sudo pacmatic -S autoconf automake make gcc ghc cabal-install \
python2 python3 vim i3 rxvt-unicode xorg-xrdb xorg-xinit dmenu \
llvm clang boost cmake ttf-dejavu bash-completion astyle git

./install-configs-copy.sh
