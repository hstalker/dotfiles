#! /usr/bin/env bash

xrdb -merge ~/.Xresources
[[ -r ~/.xinitrc.local ]] && . ~/.xinitrc.local

