#!/bin/zsh
HISTFILE=~/.histfile
HISTSIZE=30000
SAVEHIST=30000
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

setopt extendedglob notify interactivecomments \
    extendedhistory incappendhistory sharehistory histfindnodups histverify histignorespace
unsetopt autocd nomatch beep
bindkey -e

##
# plugin
##

# zgen
export ZGEN_DIR="${HOME}/.zgen/bundles"
source "${HOME}/.zgen/zgen.zsh"
if ! zgen saved; then
    echo "Creating a zgen save..."
    zgen loadall <<EOPLUGINS
        zsh-users/zsh-completions src
        zsh-users/zsh-history-substring-search
        zsh-users/zaw
        rupa/z
EOPLUGINS
    zgen save
fi

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
setopt alwaystoend completeinword automenu autolist autoparamslash listpacked
unsetopt menucomplete flowcontrol
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

##
# alias
##
alias j="z"
alias tl="tail"
alias hd="head"
alias l="less"
alias lm="ls -lahF"
alias findn="find . -name"
alias tmux="tmux -2 -u"
alias csk="brew cask"
alias ec="emacsclient -nw"
alias g="git"
alias dr=docker-run
alias dr1=docker-run1
alias drb=docker-runbg

##
# prompt
##
function prompt_precmd {
    local git_prompt=
    if [[ "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]]; then
        local branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"

        # Working tree status (red when dirty)
        local dirty=
        # Modified files
        git diff --no-ext-diff --quiet --exit-code --ignore-submodules 2>/dev/null || dirty=1
        # Untracked files
        [ -z "$dirty" ] && test -n "$(git status --porcelain)" && dirty=1

        # Format Git info
        if [ -n "$dirty" ]; then
            git_prompt=" %F{red}g:$branch%f"
        else
            git_prompt=" %F{green}g:$branch%f"
        fi
    fi

    # Virtualenv
    local venv_prompt=
    if [ -n "$VIRTUAL_ENV" ]; then
        venv_prompt=" %F{blue}v:$(basename $VIRTUAL_ENV)%f"
    fi

    PROMPT="%F{cyan}%2~%f${git_prompt}${venv_prompt} > "
}

function prompt_setup {
    prompt_opts=(cr percent)
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd prompt_precmd
}

prompt_setup "$@"
