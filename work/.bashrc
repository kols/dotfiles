if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

shopt -s dotglob
shopt -s nocaseglob
shopt -s cdspell
shopt -s checkwinsize
shopt -s histappend

HISTSIZE=25000
HISTFILESIZE=25000

asura_logdir=/data/log/sns.asura
asura_reqlog=/data/log/sns.asura/asura.log

function _asura_logfiles () {
    find $asura_logdir -name '*.log' | perl -ne 'print if /\/[^\.]+\.log/'
}

function rid_trace () {
    command grep -h $1 `_asura_logfiles` | sort -g | uniq | nl
}

function highcost () {
    local awk_expr='int($NF) >= '"${2:-500}"
    [ -z $1 ] && return 1
    grep $1 $asura_reqlog | awk "$awk_expr"
}

function return-limits () {
	for process in $@; do
		process_pids=`ps -C $process -o pid --no-headers | cut -d " " -f 2`

		if [ -z $@ ]; then
			echo "[no $process running]"
		else
			for pid in $process_pids; do
				echo "[$process #$pid -- limits]"
				cat /proc/$pid/limits
			done
		fi
	done
}

alias vi=vim
alias tl="tail"
alias hd="head"
alias l="less"
alias lm="ls -lahF"
alias l1="ls -A1"
alias findn="find . -name"
alias tmux="tmux -2 -u"
alias g="git"
alias ncpu="getconf _NPROCESSORS_ONLN"
