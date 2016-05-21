func! Markdown_Folding_expr(lnum)
    let l1 = getline(a:lnum)

    if l1 =~ '^\s*$'
        return '='
    endif

    if l1 =~ '^#'
        return '>' . matchend(l1, '^#\+')
    endif

    let l2 = getline(a:lnum+1)

    if l2 =~ '^---\s*$'
        return '='
    endif

    if l2 =~ '^---\+\s*$'
        return '>2'
    elseif l2 =~ '^===\+\s*$'
        return '>1'
    endif

    " premeable
    if a:lnum == 1 && l1 =~ '^---\s*$'
        return '>1'
    endif

    return '='
endfunc

setlocal foldexpr=Markdown_Folding_expr(v:lnum)
setlocal foldmethod=expr
