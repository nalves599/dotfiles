#!/bin/zsh

# Fix debian-based servers not having alacritty terminfo
alias ssh="TERM=xterm-256color ssh"
# Don't use vim, use nvim instead
alias vim="nvim"
# Open files
alias xopen="xdg-open"

# RNL
alias rnl="cd $HOME/Documents/RNL"
alias rnl-up="wg-quick up $HOME/.vpn/RNL.conf"
alias rnl-down="wg-quick down $HOME/.vpn/RNL.conf"
# IB
alias ib="cd $HOME/Documents/InspiredBlue"
# STT
alias stt="cd $HOME/Documents/STT"
alias stt-up="wg-quick up $HOME/.vpn/STT.conf"
alias stt-down="wg-quick down $HOME/.vpn/STT.conf"
