function js
    set srcpath (ghq list -p | fzf --reverse --height 12 --prompt "repo> " --query "$argv")
    if test -z "$srcpath"
        return
    end
    pushd "$srcpath"
end
