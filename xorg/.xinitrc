#! /usr/bin/env bash

[[ -r ~/.Xresources ]] && xrdb -merge ~/.Xresources
[[ -r ~/.xinitrc.local]] && . ~/.xinitrc.local

