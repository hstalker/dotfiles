#!/bin/sh

# Grab packages
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install autoconf gcc g++ ghc-mod cabal-install \
python python3 vim-nox i3 xorg rxvt-unicode-256color astyle git \
llvm clang libclang-dev libboost-all-dev cmake ttf-dejavu bash-completion
./install-configs-copy.sh
