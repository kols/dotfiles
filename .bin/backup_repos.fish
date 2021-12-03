#!/usr/bin/env fish

function backup
    set -l repos "$HOME/nvALT" "$HOME/org" "$HOME/ledger"
    for r in $repos
        cd $r
        echo "* Backup repo:" $r
        git status
        echo "* Stage changes:"
        git add .
        git commit --message 'update'
        set -l remotes (git remote)
        echo "* Repo remotes:" $remotes
        for re in $remotes
            echo "* Push remote:" $re
            git push --all $re
        end
    end
end

echo -s "* Backup start @" (date +%s) 
backup
set -l s $status
if test $s -eq 0
    terminal-notifier -title 'Backup repos' -message 'Success!'
else
    terminal-notifier -ignoreDnD -title 'Backup repos' -message 'FAILED!'
end
exit $s
