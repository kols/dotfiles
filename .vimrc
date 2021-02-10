" packages {{{1
packadd minpac

call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})

call minpac#add('jnurmine/Zenburn')
call minpac#add('dense-analysis/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('ctrlpvim/ctrlp.vim')
call minpac#add('neoclide/coc.nvim', {'branch': 'release'})
call minpac#add('itchyny/lightline.vim')
call minpac#add('maximbaz/lightline-ale')
call minpac#add('mhinz/vim-startify')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-obsession')
call minpac#add('nathanaelkane/vim-indent-guides')
call minpac#add('vim-scripts/peaksea')

call minpac#add('tomtom/tcomment_vim')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-repeat')
call minpac#add('chrisbra/NrrwRgn')
call minpac#add('easymotion/vim-easymotion')
call minpac#add('Raimondi/delimitMate')
call minpac#add('ervandew/supertab')
call minpac#add('majutsushi/tagbar')

call minpac#add('goerz/jupytext.vim')
call minpac#add('hkupty/iron.nvim')
call minpac#add('kana/vim-textobj-user')
call minpac#add('GCBallesteros/vim-textobj-hydrogen')
call minpac#add('wellle/targets.vim')
call minpac#add('simeji/winresizer')
call minpac#add('Vimjas/vim-python-pep8-indent')
call minpac#add('pacha/vem-tabline')

call minpac#add('christoomey/vim-tmux-navigator')
call minpac#add('Shougo/echodoc.vim')

call minpac#add('airblade/vim-rooter')
call minpac#add('jeetsukumaran/vim-filebeagle')

call minpac#add('osyo-manga/vim-over')
call minpac#add('vim-scripts/YankRing.vim')
call minpac#add('pechorin/any-jump.vim')
if !has("nvim")
  call minpac#add('bfrg/vim-jqplay')
endif

call minpac#add('Shougo/denite.nvim')
call minpac#add('Shougo/neomru.vim')
call minpac#add('liuchengxu/vim-clap')


call minpac#add('godlygeek/tabular')
call minpac#add('plasticboy/vim-markdown')

call minpac#add('skywind3000/asyncrun.vim')

packloadall!

function s:update_packages()
  call minpac#update('', {'do': 'call minpac#status()'})
endfunction
"}}}

" editing {{{1
" keymapping {{{1

let mapleader = ","

" The <Space> {{{2
map <Space> [Space]

noremap [Space] <Nop>

nnoremap <silent> [Space]/ :nohlsearch<Return>
vnoremap [Space]so y:execute @@<Return>:echo 'selection evaluated'<Return>
nnoremap [Space]so yy:execute @@<Return>:echo 'line evaluated'<Return>
"}}}

" buffer {{{2
nnoremap <silent> [Space]bh :b #\|:hide #<Return>
nnoremap <silent> <leader>q :b #\|bd #<Return>
nnoremap <silent> <leader>Q :b #\|bd! #<Return>
nnoremap <silent> <leader>c :close<Return>
"}}}

" tab {{{
nnoremap <C-t> <Nop>

nmap <silent> <C-t>n :tabnew \| tabmove<Return>
nmap <silent> <C-t>c :tabclose<Return>
nmap <silent> <C-t>C :tabclose!<Return>
nmap <silent> <C-t>o :tabonly<Return>
nmap <silent> <C-t>i :tabs<Return>

nmap <C-t><C-n> <C-t>n
nnoremap <C-t><C-c> <C-t>c
nmap <C-t><C-o> <C-t>o
nmap <C-t><C-i> <C-t>i

nmap <silent> <C-t>j :execute 'tabnext' 1 + (tabpagenr() + v:count1 - 1) % tabpagenr('$')<Return>
nmap <silent> <C-t>k :tabprevious<Return>
nmap <silent> <C-t>K :tabfirst<Return>
nmap <silent> <C-t>J :tablast<Return>

nmap <C-t><C-j> <C-t>j
nmap <C-t><C-k> <C-t>k
"}}}

" list {{{2
nnoremap <silent> ]q :cnext<Return>
nnoremap <silent> [q :cprevious<Return>
nnoremap <silent> ]l :lnext<Return>
nnoremap <silent> [l :lprevious<Return>
"}}}

" editing {{{2
nnoremap <silent> j gj
nnoremap <silent> k gk
inoremap jj <Esc>
nmap <silent> [Space]p :set paste<Return>
nmap <silent> [Space]P :set nopaste<Return>
nnoremap <C-w>V :vne<Return>
"}}}

" terminal (neovim) {{{2
if has("nvim")
  tnoremap <Esc> <C-\><C-n>
endif
"}}}
"}}}

" general {{{1
filetype plugin indent on
syntax on

if &shell =~# 'fish$'
set shell=sh
endif

set updatetime=700

if has("nvim")
  let g:python3_host_prog = '~/.pyenv/versions/3.9.1/bin/python'
  let g:python_host_prog  = '~/.pyenv/versions/2.7.18/bin/python'
endif

set switchbuf = "usetab,useopen,uselast"

" encoding {{{2
set encoding=utf-8
set fileencodings=utf-8,chinese,latin-1
"}}}

" editing {{{2
" jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
      \ exe "normal! g'\"" |
      \ endif
set nowrap
set textwidth=79
set colorcolumn=80
set backspace=indent,eol,start
set autoindent
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4
set shiftround
set modeline
set hidden
"}}}

" searching {{{2
set ignorecase
set smartcase
set incsearch
set hlsearch
"}}}

" convenience {{{2
set noautochdir
set showmatch
set autoread
set wildmenu
set wildignore+=*~,#*#
set completeopt=menu,menuone,longest
" insert completion menu items
set pumheight=15
"}}}

" folding {{{2
set nofoldenable
set foldmethod=syntax
set foldlevelstart=10
set foldnestmax=10
"}}}

" backup {{{2
set backup
set backupcopy&
set backupdir=~/.cache/vim
set directory=~/.cache/vim
"}}}

" interface {{{2
if exists('+guioptions')
  set guioptions=cgM
endif
set noequalalways
set ruler
set laststatus=2
set showtabline=2
set nonumber
set showbreak=↳
set showcmd
set mouse&
set splitbelow
set splitright
" stop flashing
set noerrorbells visualbell t_vb=
au GUIEnter * set visualbell t_vb=
set noshowmode

" Highlight end of line whitespace.
highlight WhitespaceEOL ctermbg=red guibg=red
match WhitespaceEOL /\s\+$/

" https://github.com/kana/config/blob/master/vim/personal/dot.vimrc#L189
if (1 < &t_Co || has('gui')) && has('syntax')
  set t_Co=256
  if !exists('g:colors_name')
    set background=dark
    let g:zenburn_transparent = 0
    let g:zenburn_high_Contrast = 0
    let g:zenburn_alternate_Visual = 0
    let g:zenburn_unified_CursorColumn = 1
    let g:zenburn_old_Visual = 0
    let g:zenburn_enable_TagHighlight = 1

    let g:seoul256_background = 235

    colorscheme zenburn
  endif
endif

" terminal {{{3
if has("nvim")
  augroup term
    autocmd!
    autocmd TermOpen * setlocal bufhidden=hide | setlocal buflisted
  augroup END
endif
"}}}

" buffer {{{3
augroup help
  autocmd!
  autocmd FileType help setlocal bufhidden=hide | setlocal buflisted
augroup END
"}}}
"}}}

" font {{{2
if exists('+guifont')
  set guifont=hack:h16pt
endif
"}}}
"}}}

" languages {{{1
" java {{{2
augroup java
  autocmd!
  autocmd FileType java call s:java_settings()
  function! s:java_settings() abort
    nnoremap <silent> <buffer> [Space]gd :AnyJump<Return>
    xnoremap <silent> <buffer> [Space]gd :AnyJumpVisual<Return>
  endfunction
augroup END
"}}}

" python {{{2
augroup python
  autocmd!
  autocmd FileType python
        \ setlocal colorcolumn=80 |
        \ setlocal iskeyword& |
        \ setlocal tabstop=4 expandtab softtabstop=4 shiftround shiftwidth=4 |
        \ setlocal smartindent cinwords=if,elseif,else,for,while,class |
        \ setlocal wildignore+=*.py[co] |
        \ setlocal foldmethod=indent |
        \ setlocal formatoptions=cq textwidth=72 foldignore=

  autocmd FileType python nmap <silent> <buffer> [Space]gd <Plug>(coc-definition)
  autocmd FileType python nmap <silent> <buffer> [Space]gr <Plug>(coc-references)
  autocmd FileType python nmap <silent> <buffer> [Space]d :call CocActionAsync('doHover')<Return>
  autocmd FileType python nmap <silent> <buffer> <leader>rn <Plug>(coc-rename)
  autocmd FileType python nmap <silent> <buffer> <leader>A <Plug>(coc-codeaction)
  autocmd FileType python nmap <silent> <buffer> <leader>a <Plug>(coc-codeaction-line)
        " \ nmap <silent> <buffer> <M-CR> <Plug>(coc-codeaction-line) |
        " \ nmap <silent> <buffer> <M-S-CR> <Plug>(coc-codeaction)
  autocmd FileType python nmap <silent> <buffer> [Space]ec :CocCommand python.runLinting<Return>
augroup END
"}}}

" vimscript {{{2
augroup vim
  autocmd!
  autocmd FileType vim nmap <silent> <buffer> [Space]gd <Plug>(coc-definition)
  autocmd FileType vim nmap <silent> <buffer> [Space]gr <Plug>(coc-references)
  autocmd FileType vim nmap <silent> <buffer> [Space]d :call CocActionAsync('doHover')<Return>
  autocmd FileType vim nmap <silent> <buffer> <leader>rn <Plug>(coc-rename)
augroup END
" }}}

" go {{{2
augroup go
  autocmd!
  autocmd FileType go setlocal colorcolumn=
augroup END
" }}}

" ruby {{{2
augroup ruby
  autocmd!
  autocmd BufRead,BufNewFile *.rb setfiletype ruby
  autocmd FileType ruby setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround
augroup END
"}}}

" c {{{2
augroup c
  autocmd!
  autocmd FileType c setlocal tabstop=4 expandtab softtabstop=4 shiftwidth=4 shiftround
augroup END
"}}}

" html {{{2
augroup html
  autocmd!
  autocmd BufRead,BufNewFile *.html setfiletype html
  autocmd FileType html setlocal tabstop=4 expandtab softtabstop=4 shiftwidth=4 shiftround
augroup END
"}}}

" php {{{2
augroup php
  autocmd!
  autocmd BufRead,BufNewFile *.php setfiletype php
  autocmd FileType php setlocal tabstop=4 expandtab softtabstop=4 shiftwidth=4 shiftround
augroup END
"}}}

" javascript {{{2
augroup javascript
  autocmd!
  autocmd FileType javascript setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround

  autocmd BufRead,BufNewFile *.coffee setfiletype coffeescript
  autocmd FileType coffeescript setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround
augroup END
"}}}

" markdown {{{2
augroup markdown
  autocmd!
  function! s:markdown_settings() abort
    setlocal textwidth=79
    setlocal conceallevel=2
    setlocal wrap
    nnoremap <silent> <buffer> [Space]o :AsyncRun open -a Marked\ 2 %<Return>
    nnoremap <silent> <buffer> gO :Toch<Return>
  endfunction
  autocmd FileType markdown call s:markdown_settings()
augroup END
"}}}

" yaml {{{2
augroup yaml
  autocmd!
  autocmd BufRead,BufNewFile *.yaml,*.yml setfiletype yaml
  autocmd FileType yaml setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround
augroup END
"}}}
"}}}

" plugin/script {{{1
nnoremap <leader>pu :call <SID>update_packages()<Return>

" Shougo/denite.nvim {{{2
nnoremap <silent> [Space]bb :Denite -start-filter -auto-resize -winheight=12 buffer<Return>
nnoremap <silent> <C-p> :Denite -start-filter -auto-resize -winheight=12 file/rec<Return>
nnoremap <silent> [Space]zr :Denite -start-filter -auto-resize -winheight=12 file/old<Return>
nnoremap <silent> [Space]/ :Denite -start-filter line<Return>

call denite#custom#option('_', 'prompt', '> ')
call denite#custom#option('_', 'statusline', v:false)
call denite#custom#filter('matcher/clap', 'clap_path', expand('~/.vim/pack/minpac/start/vim-clap'))
for src in ['file/old', 'file/rec', 'file_mru', 'buffer', 'line']
  call denite#custom#source(src, 'matchers', ['matcher/clap'])
endfor

augroup denite_settings
  autocmd!
  autocmd FileType denite call s:denite_my_settings()
  function! s:denite_my_settings() abort
    nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
    nnoremap <silent><buffer><expr> d denite#do_map('do_action', 'delete')
    nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
    nnoremap <silent><buffer><expr> q denite#do_map('quit')
    nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
    nnoremap <silent><buffer><expr> g denite#do_map('quick_move')
    nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
  endfunction

  autocmd FileType denite-filter call s:denite_filter_settings()
  function! s:denite_filter_settings() abort
    inoremap <silent><buffer><expr> <C-j> denite#increment_parent_cursor(1)
    inoremap <silent><buffer><expr> <C-k> denite#increment_parent_cursor(-1)
    nnoremap <silent><buffer><expr> <C-j> denite#increment_parent_cursor(1)
    nnoremap <silent><buffer><expr> <C-k> denite#increment_parent_cursor(-1)
    inoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
    nnoremap <silent><buffer><expr> <C-c> denite#do_map('quit')
    inoremap <silent><buffer><expr> <C-c> denite#do_map('quit')
    inoremap <silent><buffer><expr> <Esc> denite#do_map('quit')
    nnoremap <silent><buffer><expr> <Esc> denite#do_map('quit')
  endfunction
augroup END
"}}}

" benmills/vimux {{{2
nnoremap [Space]vp :VimuxPromptCommand<Return>
nnoremap [Space]vl :VimuxRunLastCommand<Return>
nnoremap [Space]vi :VimuxInspectRunner<Return>
"}}}

" xolox/vim-easytags {{{2
set cpoptions+=d
set tags=./tags
let g:easytags_file = ''
let g:easytags_dynamic_files = 2
let g:easytags_async = 1
let g:easytags_auto_highlight = 0
augroup go
  autocmd FileType go let b:easytags_autorecurse = 1
augroup END

augroup python
  autocmd FileType python let b:easytags_autorecurse = 1
augroup END
" }}}

" ervandew/supertab {{{2
let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestEnhanced = 1
"}}}

" netrw {{{2
let g:netrw_altv = 1
let g:netrw_banner = 0
let g:netrw_browse_split = 4
let g:netrw_liststyle = 0
let g:netrw_winsize = -28
let g:netrw_bufsettings="noma nomod nu nobl nowrap ro nornu"
"}}}

" nerdtree {{{2
let NERDTreeQuitOnOpen = 1
let NERDTreeShowBookmarks = 1
let NERDTreeHijackNetrw = 0
let NERDTreeMinimalUI = 1

augroup python
  autocmd filetype python let NERDTreeIgnore = ['\.pyc$']
augroup END

map <leader>7 <ESC><ESC>:NERDTreeToggle<Return>
map <leader>8 <ESC><ESC>:NERDTreeFind<Return>
"}}}

" undotree {{{2
nnoremap [Space]u :UndotreeToggle<Return>
"}}}

" w0rp/ale {{{2
let g:ale_disable_lsp = 1
let s:format = '--format="{{.Path}}:{{.Line}}:{{if .Col}}{{.Col}}{{end}}: {{.Severity}}: {{.Message}} ({{.Linter}})"'
let g:ale_linter_aliases = {
\   'sls': 'yaml',
\}
let g:ale_sign_column_always = 1
let g:ale_lint_on_enter = 0
let g:ale_lint_on_text_changed = 1
let g:ale_lint_on_save = 1

" python
let g:ale_virtualenv_dir_names = []
map <silent> [Space]ec :call ale#Queue(0)<Return>
nnoremap <silent> [Space]eo :lopen<Return>
"}}}

" tagbar {{{2
nnoremap <silent> [Space]' :TagbarToggle<Return>
let g:tagbar_autofocus = 1
let g:tagbar_sort = 0
let g:tagbar_compact=1
let g:tagbar_autoshowtag = 1
let g:tagbar_type_go = {
	\ 'ctagstype' : 'go',
	\ 'kinds'     : [
		\ 'p:package',
		\ 'i:imports:1',
		\ 'c:constants',
		\ 'v:variables',
		\ 't:types',
		\ 'n:interfaces',
		\ 'w:fields',
		\ 'e:embedded',
		\ 'm:methods',
		\ 'r:constructor',
		\ 'f:functions'
	\ ],
	\ 'sro' : '.',
	\ 'kind2scope' : {
		\ 't' : 'ctype',
		\ 'n' : 'ntype'
	\ },
	\ 'scope2kind' : {
		\ 'ctype' : 't',
		\ 'ntype' : 'n'
	\ },
	\ 'ctagsbin'  : 'gotags',
	\ 'ctagsargs' : '-sort -silent'
\ }
let g:tagbar_type_thrift = {
	\ 'ctagstype' : 'thrift',
	\ 'kinds'     : [
		\ 'n:namespace',
		\ 'i:include:1',
		\ 'c:constant',
		\ 'v:service',
		\ 'x:exception',
		\ 'e:enum',
		\ 's:struct',
		\ 'm:member',
		\ 'a:value',
		\ 'f:function',
	\ ],
	\ 'sro' : '{',
	\ 'kind2scope' : {
	\ },
	\ 'scope2kind' : {
	\ },
\ }
"}}}

" mhinz/vim-grepper {{{2
runtime plugin/grepper.vim
nnoremap <silent> [Space]a :Grepper -highlight -noprompt -cword<Return>
nnoremap [Space]A :Grepper<Return>
"}}}

" ctrlpvim/ctrlp.vim {{{2
" let g:ctrlp_map = '<C-p>'
" let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_match_window = 'bottom,top,order:ttb,min:1,max:15,results:12'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = 0
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](node_modules|bower_components)$',
  \ }
let g:ctrlp_persistent_input = 0
let g:ctrlp_user_command = 'rg %s -l --color never --files'
let g:ctrlp_extensions = ['tag', 'buffertag']
let g:ctrlp_buftag_types = {
    \ 'thrift'     : '--language-force=thrift',
    \ }

augroup go
  autocmd FileType go let b:ctrlp_buftag_ctags_bin="gotags"
augroup END

" nnoremap <silent> <C-p> :CtrlP<Return>
nnoremap <silent> [Space]f :CtrlPBufTag<Return>
nnoremap <silent> [Space]t :CtrlPTag<Return>
" nnoremap <silent> [Space]zr :CtrlPMRU<Return>
" nnoremap <silent> [Space]bb :CtrlPBuffer<Return>
"}}}

" maralla/completor.vim {{{2
let g:completor_python_binary = '/usr/local/bin/python'
let g:completor_blacklist = ['java', 'tagbar', 'netrw']
"}}}

" lambdalisue/vim-pyenv {{{2
let g:pyenv#auto_activate = 1
"}}}

" davidhalter/jedi-vim {{{2
" let g:jedi#auto_initialization = 0
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#completions_enabled = 0
" let g:jedi#show_call_signatures = "2"
" augroup python
"   autocmd FileType python
"         \ nnoremap <silent> <buffer> [Space]gg :call jedi#goto()<Return> |
"         \ nnoremap <silent> <buffer> [Space]gd :call jedi#goto_assignments()<Return>
" augroup END
"}}}

" ultisnips {{{2
let g:UltiSnipsExpandTrigger="<C-d>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"
"}}}

" indent-guides {{{2
let g:indent_guides_enable_on_vim_startup = 0
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
let g:indent_guides_color_change_percent = 5
"}}}

" test {{{2
let test#strategy = 'vimux'
augroup python
  autocmd FileType python let test#python#runner = 'pytest'
augroup END
nmap <silent> <leader>tn :TestNearest<Return>
nmap <silent> <leader>tf :TestFile<Return>
nmap <silent> <leader>ta :TestSuite<Return>
nmap <silent> <leader>tl :TestLast<Return>
nmap <silent> <leader>tg :TestVisit<Return>
"}}}

" itchyny/lightline.vim {{{2
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'session', 'repo', 'filename', 'modified', 'fugitive' ],
      \             [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype' ],
      \              [ 'coc' ] ],
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"%%":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"**":&modifiable?"":"--"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}',
      \   'time': '%{strftime("%H:%M")}',
      \   'repo': '%{exists("*FindRootDirectory")?fnamemodify(FindRootDirectory(), ":t:."):""}',
      \   'session': '%{exists("*ObsessionStatus")&&""!=ObsessionStatus()?ObsessionStatus("~", "-"):"-"}',
      \   'coc': 'coc#status',
      \ },
      \ 'component_function': {
      \   'coc': 'coc#status',
      \ },
      \ 'component_visible_condition': {
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())',
      \   'repo': '(exists("*FindRootDirectory") && ""!=FindRootDirectory())',
      \   'session': '(exists("*ObsessionStatus"))',
      \ },
      \ 'component_expand': {
      \   'linter_checking': 'lightline#ale#checking',
      \   'linter_infos': 'lightline#ale#infos',
      \   'linter_warnings': 'lightline#ale#warnings',
      \   'linter_errors': 'lightline#ale#errors',
      \   'linter_ok': 'lightline#ale#ok',
      \ },
      \ 'component_type': {
      \   'linter_checking': 'right',
      \   'linter_infos': 'right',
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error',
      \   'linter_ok': 'right',
      \   'ale': 'error',
      \ },
      \ }
let g:lightline.enable = {
    \ 'statusline': 1,
    \ 'tabline': 0
    \ }
call lightline#init() | call lightline#update()
"}}} 

" vim-go {{{2
let g:go_list_type = "quickfix"
let g:go_def_mapping_enabled=0
let g:go_fmt_command="goimports"
augroup go
  autocmd FileType go nnoremap <silent> <buffer> [Space]gg :GoDef<Return>
  autocmd FileType go nnoremap <silent> <buffer> <leader>r :GoRun<Return>
augroup END
"}}}

" vim-rooter {{{2
let g:rooter_change_directory_for_non_project_files = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
let g:rooter_cd_cmd = 'lcd'
"}}}

" filebeagle {{{2
let g:filebeagle_check_gitignore = 1
let g:filebeagle_show_hidden = 1
let g:filebeagle_show_line_numbers = 1
map <silent> - <Plug>FileBeagleOpenCurrentBufferDir
"}}}

" rust.vim {{{2
let g:rustfmt_autosave = 1
"}}}

" vimagit {{{2
" nnoremap [Space]vs :Magit<Return>
"}}}

" tpope/fugitive {{{2
nnoremap [Space]vs :Git<Return>
nnoremap [Space]vb :Git blame<Return>
nnoremap [Space]vr :on \| :Gvdiffsplit!<Return>
nnoremap <leader>vra :diffoff! \| :on \| :w \| :Git<Return>
nnoremap [Space]doa :diffget //2<Return>
nnoremap [Space]dob :diffget //3<Return>
"}}}

" easymotion/vim-easymotion {{{2
let g:EasyMotion_do_mapping = 1
"}}}

" coc.nvim {{{2
autocmd CursorHold * silent call CocActionAsync('highlight')
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
"}}}

" hkupty/iron.nvim {{{2
luafile $HOME/.config/nvim/plugins.lua
"}}}

" goerz/jupytext.vim {{{2
let g:jupytext_fmt = 'py:percent'
"}}}

" pacha/vem-tabline {{{2 
let g:vem_tabline_show = 2
let g:vem_unnamed_buffer_label = "~"
let g:vem_tabline_show_number = "none"
let g:vem_tabline_multiwindow_mode = 1
"}}}

" simeji/winresizer {{{2
let g:winresizer_vert_resize=6
"}}}

" hkupty/iron.nvim {{{2
map <leader>sb ctrah
imap <leader>sb <Esc>ctrah
"}}}

" {{{2
let g:yankring_replace_n_pkey = '<c-y>'
"}}}
" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
