HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=500000
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

# zplugin
source ~/.zinit/bin/zinit.zsh

# zinit load "zinit-zsh/z-a-bin-gem-node"

zinit load "zsh-users/zsh-completions"
zinit load "zsh-users/zsh-history-substring-search"
zinit load "zsh-users/zsh-syntax-highlighting"
zinit load "zsh-users/zsh-autosuggestions"

zinit ice as"program" from"gh-r"
zinit load "junegunn/fzf"

zinit ice as"program" from"gh-r" mv"jq* -> jq"
zinit load "stedolan/jq"

zinit ice as"program" from"gh-r" pick"ghq_*/ghq"
zinit load "x-motemen/ghq"

zinit ice as"program" pick"git-icdiff"
zinit load "jeffkaufman/icdiff"

zinit ice as"program" from"gh-r" mv"sops* -> sops" bpick"*darwin*"
zinit load "mozilla/sops"

zinit ice from"gh-r" as"program" pick"zoxide-*-x86_64-apple-darwin/zoxide"
zinit load "ajeetdsouza/zoxide"

zinit ice from"gh-r" as"program" pick"bin/exa"
zinit load "ogham/exa"

zinit ice from"gh-r" as"program"
zinit load "zigtools/zls"

HISTDB_TABULATE_CMD=(sed -e $'s/\x1f/\t/g')
zinit ice depth"1" # git clone depth
zinit load "larkery/zsh-histdb"

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
    PYENV_VERSION="2" pyenv exec python -c 'import os, hashlib; print hashlib.sha256(os.urandom(2048)).hexdigest()[:7]'
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
alias b=brew
alias c=clear
alias ls=exa
alias vi=vim
alias mvim='_mvim_open_in_tab'
alias j="z"
alias tl="tail"
alias hd="head"
alias l="less"
alias lm="ls -lahF"
alias km="k -ah"
alias l1="ls -1"
alias findn="find . -name"
alias tmux="tmux -2 -u"
alias ta="tmux attach"
alias csk="brew cask"
alias ec="open -a Emacs.app"
alias em="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n"
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
alias wghq='GHQ_ROOT=~/work/repos ghq'
## nvim alias
if command -v nvim &>/dev/null; then
    alias vim=nvim
    alias vi=nvim
fi

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
		return
	fi
    pushd "$srcpath"
}

# jog
function jog() {
    sqlite3 $HOME/.histdb/zsh-history.db "
SELECT
    replace(commands.argv, '
', '
')
FROM commands
JOIN history ON history.command_id = commands.id
JOIN places ON history.place_id = places.id
WHERE history.exit_status = 0
AND dir = '${PWD}'
AND places.host = '${HOST}'
AND commands.argv != 'jog'
AND commands.argv NOT LIKE 'z %'
AND commands.argv NOT LIKE 'cd %'
AND commands.argv != '..'
ORDER BY start_time DESC
LIMIT 10
"
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
		return
	fi
	ssh "$host" "$opts"
}

if [ -f ~/.zshrc.custom ]; then
    . ~/.zshrc.custom
fi

function install_pypkg {
    PYENV_VERSION=`pyenv global` pyenv exec pip install -r $HOME/requirements.txt "$1"
}

function install_gopkg {
    local pkgs=(
        # linter
        github.com/alecthomas/gometalinter
        github.com/golang/lint/golint
        github.com/jgautheron/goconst/cmd/goconst
        github.com/opennota/check/cmd/varcheck
        github.com/opennota/check/cmd/aligncheck
        github.com/opennota/check/cmd/structcheck
        golang.org/x/tools/cmd/gotype

        # auto complete
        github.com/nsf/gocode

        # formatting
        golang.org/x/tools/cmd/goimports

        # tagging
        github.com/jstemmer/gotags
        github.com/rogpeppe/godef

        # analysis
        golang.org/x/tools/cmd/guru
        github.com/jstemmer/gotags
        github.com/juntaki/gogtags

        # dependency
        github.com/golang/dep/cmd/dep

        # build
        github.com/mitchellh/gox

        # exec
        github.com/motemen/github-list-starred
    )
    for p in $pkgs; do
        go get -v -u $p
    done
}

# python
export VIRTUAL_ENV_DISABLE_PROMPT=1

# zoxide
eval "$(zoxide init zsh)"

# starship
eval "$(starship init zsh)"

# vim:ft=zsh
