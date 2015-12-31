#!/bin/sh
export LANG='en_US.UTF-8'
export LC_CTYPE=$LANG
export PATH=$HOME/Library/Python/2.7/bin:$PATH:$HOME/bin
export EDITOR=/opt/pkg/bin/vim
export VISUAL=$EDITOR

# go
export GOROOT=/opt/pkg/go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
