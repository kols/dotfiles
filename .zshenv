#!/usr/bin/env zsh
# PATH
export PATH=/usr/local/sbin:/usr/local/bin:$PATH:$HOME/bin
export LANG='en_US.UTF-8'
export EDITOR='/usr/local/bin/vim'
export VISUAL=$EDITOR

# app
export NODE_PATH=/usr/local/lib/node_modules
export NVM_DIR=~/.nvm

export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin

export PLAN9=/usr/local/plan9
export PATH=$PATH:$PLAN9/bin
