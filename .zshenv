#!/bin/sh

export LANG='en_US.UTF-8'
export LC_CTYPE=$LANG
export PATH=$HOME/Library/Python/2.7/bin:$PATH:$HOME/bin
export EDITOR=/opt/pkg/bin/vim
export VISUAL=$EDITOR
alias vim=$EDITOR
alias vi=vim
alias git=/opt/pkg/bin/git

export GOROOT=/opt/pkg/go/
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin

# virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=1
# virtualenvwrapper¬
export WORKON_HOME=~/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
