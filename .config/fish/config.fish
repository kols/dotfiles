set fish_greeting

fenv "source /etc/profile"
fenv "source ~/.zprofile"
status --is-interactive; fenv "source ~/.zshrc 2>/dev/null"

# make sure envvar SHELL is the right one after loading zsh files above
set -gx SHELL /usr/local/bin/fish

alias j=z
alias b=brew
alias vi=nvim
alias vim=nvim
alias tl="tail"
alias hd="head"
alias l="less"
alias ls=exa
alias lm="ls -lahF"
alias l1="ls -1a"
alias ec="emacsclient"
alias g="git"
alias ncpu="getconf _NPROCESSORS_ONLN"
alias rcopy="rsync -avz --progress -h"
alias rmove="rsync -avz --progress -h --remove-source-files"
alias rupdate="rsync -avzu --progress -h"
alias rsync-synchronize="rsync -avzu --delete --progress -h"

# starship
starship init fish | source
