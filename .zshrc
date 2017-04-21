export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case --quit-if-one-screen --RAW-CONTROL-CHARS'

# icloud drive
export icloud=~/Library/Mobile\ Documents/com\~apple\~CloudDocs

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=50000
WORDCHARS='*?_.[]~&;!#$%^(){}<>'

setopt auto_resume
setopt extendedglob
setopt notify
setopt interactive_comments
setopt extended_history
setopt inc_append_history
setopt share_history
setopt hist_find_no_dups
setopt hist_ignore_dups
setopt hist_reduce_blanks
setopt hist_verify
setopt hist_ignore_space
setopt brace_ccl
setopt correct
setopt long_list_jobs
setopt magic_equal_subst
setopt print_exit_value
setopt hash_cmds
setopt rm_star_wait
setopt short_loops

unsetopt autocd
unsetopt nomatch
unsetopt beep
unsetopt flow_control

bindkey -e

##
# plugin
##

# zplug
export ZPLUG_HOME=$HOME/.zplug
source $ZPLUG_HOME/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug supercrabtree/k
zplug zsh-users/zsh-completions, use:src
zplug zsh-users/zsh-history-substring-search
zplug zsh-users/zsh-syntax-highlighting
zplug zsh-users/zsh-autosuggestions
zplug rupa/z, use:z.sh
zplug k4rthik/git-cal, as:command
zplug github/hub, from:gh-r, as:command
zplug ezekg/xo, from:gh-r, as:command, use:*darwin_amd64*
zplug peco/peco, from:gh-r, as:command, use:*darwin_amd64*
zplug stedolan/jq, from:gh-r, as:command, rename-to:jq, use:*osx-amd64*
zplug b4b4r07/emoji-cli, on:stedolan/jq
zplug mrowa44/emojify, as:command, use:emojify
zplug motemen/ghq, from:gh-r, as:command, use:*darwin_amd64*
zplug openshift/source-to-image, from:gh-r, as:command, rename-to:s2i, use:*darwin-amd64*
zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
zplug jeffkaufman/icdiff, use:{git-icdiff,icdiff}, as:command

if ! zplug check --verbose; then
    printf "install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

##
# key binding
##
zmodload zsh/terminfo
# history-substring-search
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M emacs "$terminfo[kcbt]" reverse-menu-complete
bindkey -M emacs "\e#" pound-insert

##
# color
##
autoload -U colors; colors
export CLICOLOR=1
export LSCOLORS='exfxcxdxbxGxDxabagacad'
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'

##
# completion
##
setopt alwaystoend
setopt completeinword
setopt automenu
setopt autolist
setopt autoparamslash
setopt listpacked

unsetopt menucomplete

zstyle ':completion:*:*:*:*:*' menu select
# use cache
zstyle ':completion::complete:*' use-cache on
# fuzzy match mistyped completions
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

##
# helper
##

# random hash
function random_short_hash {
    python -c 'import os, hashlib; print hashlib.sha256(os.urandom(2048)).hexdigest()[:7]'
}

# bc - An arbitrary precision calculator language
function = {
    echo "$@" | bc -l
}
alias calc==

function =2 {
    = "obase=2; $@"
}

function =16 {
    = "obase=16; $@"
}

# `du` prettified
function duf {
    du -sk "$@" | sort -n | while read size fname; do
    for unit in k M G T P E Z Y; do
        if [ $size -lt 1024 ]; then
            echo -e "${size}${unit}\t${fname}"
            break
        fi
        size=$((size/1024))
    done
done
}

# docker
function docker-clear {
    docker images -f dangling=true -q | xargs -I{} docker rmi -f {}
}

function docker-run {
    docker run -it -P -v ${HOME}:/home/kane "$@"
}

function docker-run1 {
    docker run -it --rm -P -v ${HOME}:/home/kane "$@"
}

function docker-runbg {
    docker run -d -P -v ${HOME}:/home/kane "$@"
}

# find process
function fps { ps aux | grep "$1" }

# get ssl fingerprint, arg 1 is <host>:<port>
function sslfp {
    openssl s_client -connect $1 < /dev/null 2>/dev/null | openssl x509 -fingerprint -noout -in /dev/stdin
}

function lo {
    $* "$(`fc -ln -1` | fzy)"
}

function _mvim_open_in_tab {
    mvim --remote-tab-silent "${@:-.}"
}

##
# alias
##
alias vi=vim
alias mvim='_mvim_open_in_tab'
alias j="z"
alias tl="tail"
alias hd="head"
alias l="less"
alias lm="ls -lahF"
alias km="k -ah"
alias l1="ls -A1"
alias findn="find . -name"
alias tmux="tmux -2 -u"
alias ta="tmux attach"
alias csk="brew cask"
alias ec="open -a Emacs.app"
alias g="git"
alias ncpu="getconf _NPROCESSORS_ONLN"
alias dr=docker-run
alias dr1=docker-run1
alias drb=docker-runbg
alias rcopy="rsync -avz --progress -h"
alias rmove="rsync -avz --progress -h --remove-source-files"
alias rupdate="rsync -avzu --progress -h"
alias rsync-synchronize="rsync -avzu --delete --progress -h"
alias gg='__gg () { open "https://www.google.com/search?q=$*" }; __gg'

# Improve terminal title
case "${TERM}" in
    kterm*|xterm*|vt100)
        precmd() {
            echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
        }
        ;;
esac

# jump to source dir managed by _motemen/ghq_
function js {
    local srcpath=$(ghq list -p \
        | fzy --prompt "repo> " --query "$*")
	if [ -z "$srcpath" ]; then
		return 1
	fi
    pushd "$srcpath"
}

# ssh interactively select host
function sshi {
	local q="$1"
	if [ -n "$q" ]; then
		shift
		local opts="$*"
	fi
	local host=$(cut -d' ' -f1 ~/.ssh/known_hosts \
		| sort \
		| fzy --prompt 'host> ' --query "$q")
	if [ -z "$host" ]; then
		return 1
	fi
	ssh "$host" "$opts"
}

if [ -f ~/.zshrc.custom ]; then
    . ~/.zshrc.custom
fi

# pyenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

function install_pypkg {
    PYENV_VERSION=`pyenv global` pyenv exec pip install -r $HOME/requirements.txt
}
# vim:ft=zsh
