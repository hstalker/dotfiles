#! /usr/bin/env bash

# stack these setting on top of .profile if available
# (bash ignores .profile if .bashrc exists)
[[ -f ${HOME}/.profile ]] && . ${HOME}/.profile
# stack these setting on top of .bash_login if available
# (bash ignores .bash_login if .bash_profile exists)
[[ -f ${HOME}/.bash_login ]] && . ${HOME}/.bash_login

# if interactive login shell, source bashrc
[[ -f ${HOME}/.bashrc ]] && . ${HOME}/.bashrc

