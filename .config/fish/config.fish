set fish_greeting

set -x LANG 'en_US.UTF-8'
set -x LC_CTYPE $LANG
set -x PATH $HOME/Library/Python/2.7/bin /usr/local/sbin $PATH $HOME/bin
set -x EDITOR /usr/local/bin/vim
set -x VISUAL $EDITOR

# go
set -x GOROOT /usr/local/opt/go/libexec
set -x GOPATH $HOME/go
set -x PATH $PATH $GOPATH/bin $GOROOT/bin

# rbenv
status --is-interactive; and . (rbenv init -|psub)

# virtualenv
eval (python -m virtualfish compat_aliases auto_activation)

# icloud drive
set -x icloud ~/Library/Mobile\ Documents/com\~apple\~CloudDocs

# z.fish
source ~/devel/z.fish/z.fish

alias j=z
alias vi=vim
alias tl="tail"
alias hd="head"
alias l="less"
alias lm="ls -lahF"
alias l1="ls -A1"
alias findn="find . -name"
alias csk="brew cask"
alias ec="open -a Emacs.app"
alias g="git"
alias ncpu="getconf _NPROCESSORS_ONLN"
alias rcopy="rsync -avz --progress -h"
alias rmove="rsync -avz --progress -h --remove-source-files"
alias rupdate="rsync -avzu --progress -h"
alias rsync-synchronize="rsync -avzu --delete --progress -h"

# git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_color_branch red
