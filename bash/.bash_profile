#! /usr/bin/env bash

# stack these setting on top of .profile if available
# (bash ignores .profile if .bashrc exists)
[[ -f ~/.profile ]] && source ~/.profile
# stack these setting on top of .bash_login if available
# (bash ignores .bash_login if .bash_profile exists)
[[ -f ~/.bash_login ]] && source ~/.bash_login

# if interactive login shell, source bashrc
[[ -f ~/.bashrc ]] && source ~/.bashrc

[[ -f ~/.bash_profile.local ]] && source ~/.bash_profile.local

