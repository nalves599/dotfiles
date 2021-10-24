#!/bin/zsh

# Fix debian-based servers not having alacritty terminfo
alias ssh="TERM=xterm-256color ssh"
# Don't use vim, use nvim instead
alias vim="nvim"
# Open files
alias xopen="xdg-open"

# RNL
alias rnl="cd $HOME/Develop/RNL"
alias rnl-up="wg-quick up $HOME/.vpn/RNL.conf"
alias rnl-down="wg-quick down $HOME/.vpn/RNL.conf"
