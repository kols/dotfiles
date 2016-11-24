source ~/.vim/bundle.vim

" neovim compat {{{1
" make `<C-h>` work as expected
if has('nvim')
  nmap <Backspace> <C-w>h
  let g:python_host_prog = '/usr/bin/python'
endif
"}}}

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
nnoremap <silent> [Space]bq :bdelete<Return>
nnoremap <silent> [Space]bh :hide<Return>
nnoremap <silent> <leader>q :bp\|bd #<Return>
"}}}

" window {{{2
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"}}}

" tab {{{2
nnoremap <C-t> <Nop>

nmap <silent> <C-t>n :tabnew \| tabmove<Return>
nmap <silent> <C-t>c :tabclose<Return>
nmap <silent> <C-t>C :tabclose!<Return>
nmap <silent> <C-t>o :tabonly<Return>
nmap <silent> <C-t>i :tabs<Return>

nmap <C-t><C-n> <C-t>n
nmap <C-t><C-c> <C-t>c
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
inoremap jj <Esc>
inoremap <C-e> <Esc>A
nmap <silent> [Space]p :set paste<Return>
nmap <silent> [Space]P :set nopaste<Return>
"}}}

" terminal (neovim) {{{2
if has("nvim")
  tnoremap <Esc> <C-\><C-n>
  tnoremap jj <C-\><C-n>
endif
"}}}

" general {{{1
filetype plugin indent on
syntax on

if &shell =~# 'fish$'
set shell=sh
endif

" encoding {{{2
set encoding=utf-8
set fileencodings=utf-8,chinese,latin-1
"}}}

" editing {{{2
" jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
      \ exe "normal! g'\"" |
      \ endif
set wrap
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
set autochdir
set showmatch
set autoread
set wildmenu
set wildignore+=*~,#*#
set completeopt=menu,menuone
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
set showtabline=1
set number
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
    let g:zenburn_high_Contrast = 1
    let g:zenburn_alternate_Visual = 0
    let g:zenburn_unified_CursorColumn = 1
    let g:zenburn_old_Visual = 1
    let g:zenburn_enable_TagHighlight = 1
    colorscheme zenburn
  endif
endif
"}}}

" font {{{2
if exists('+guifont')
  set guifont=hack:h16pt antialias
endif
"}}}
"}}}

" languages {{{1
" python {{{2
augroup python
  autocmd!
  autocmd BufRead,BufNewFile *.py setfiletype python
  autocmd FileType python
        \ setlocal colorcolumn=+1,80 |
        \ setlocal iskeyword& |
        \ setlocal tabstop=4 expandtab softtabstop=4 shiftround shiftwidth=4 |
        \ setlocal smartindent cinwords=if,elseif,else,for,while,class |
        \ setlocal wildignore+=*.py[co] |
        \ setlocal foldmethod=indent |
        \ setlocal formatoptions=cq textwidth=72 foldignore=
augroup END
"}}}

" go {{{2
augroup go
  autocmd!
  autocmd BufRead,BufNewFile *.go setfiletype go
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
  autocmd BufRead,BufNewFile *.c,*.h setfiletype c
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
  autocmd BufRead,BufNewFile *.js setfiletype javascript
  autocmd BufRead,BufNewFile *.json setfiletype json
  autocmd FileType javascript setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround

  autocmd BufRead,BufNewFile *.coffee setfiletype coffeescript
  autocmd FileType coffeescript setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround
augroup END
"}}}

" markdown {{{2
augroup markdown
  autocmd!
  autocmd BufRead,BufNewFile *.md,*.mdwn setfiletype markdown
  autocmd FileType markdown
        \ setlocal formatoptions=tcqnmM |
        \ setlocal textwidth=79 |
        \ setlocal conceallevel=2
  autocmd FileType markdown nnoremap <silent> <buffer> [Space]o :!open -a Marked\ 2.app %<Return><Return>
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
" ervandew/supertab {{{2
let g:SuperTabDefaultCompletionType = "<c-n>"
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
let g:ale_linters = {
\   'python': ['flake8'],
\}
let g:ale_sign_column_always = 1
let g:ale_lint_on_enter = 0
let g:ale_lint_on_text_changed = 1
let g:ale_lint_on_save = 1
nnoremap <silent> [Space]ec :call ale#Queue(0)<Return>

" tagbar {{{2
nnoremap <silent> [Space]tg :Tagbar<Return>
let g:tagbar_compact=1
"}}}

" mhinz/vim-grepper {{{2
nnoremap <silent> <leader>a :Grepper -highlight -noprompt -cword<Return>
nnoremap <leader>A :Grepper<Return>
"}}}

" ctrl-p {{{2
let g:ctrlp_match_window = 'top,order:ttb,min:1,max:15,results:12'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](node_modules|bower_components)$',
  \ }
let g:ctrlp_persistent_input = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_extensions = ['buffertag']

nnoremap <silent> <C-p> :CtrlP<Return>
nnoremap <silent> <leader>f :CtrlPBufTag<Return>

" ctrlp-ghq {{{3
let ctrlp_ghq_default_action = 'e'
"}}}
"}}}

" python-mode {{{2
" rope {{{3
let g:pymode_rope = 1
let g:pymode_rope_completion = 0
let g:pymode_rope_rename_bind = '<C-c>rr'
let g:pymode_rope_rename_module_bind = '<C-c>r1r'
let g:pymode_rope_organize_imports_bind = '<C-c>ro'
let g:pymode_rope_autoimport_bind = '<C-c>ra'
let g:pymode_rope_extract_method_bind = '<C-c>rm'
let g:pymode_rope_extract_variable_bind = '<C-c>rl'
let g:pymode_rope_use_function_bind = '<C-c>ru'
" }}}
let g:pymode_virtualenv = 0
let g:pymode_indent = 1
let g:pymode_folding = 0
let g:pymode_breakpoint = 0
let g:pymode_lint = 0
let g:pymode_doc = 1
let g:pymode_doc_bind = ''
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
let test#strategy = 'basic'
augroup python
  autocmd FileType python let test#python#runner = 'pytest'
augroup END
nmap <silent> <leader>tn :TestNearest<Return>
nmap <silent> <leader>tf :TestFile<Return>
nmap <silent> <leader>ta :TestSuite<Return>
nmap <silent> <leader>tl :TestLast<Return>
nmap <silent> <leader>tg :TestVisit<Return>
"}}}

" Dispatch {{{2
augroup python
  autocmd FileType python let b:dispatch = 'py.test %'
augroup END
nnoremap <leader>i :Dispatch<Return>
nnoremap <leader>k :Make<space>
nnoremap <leader>s :Dispatch<space>
"}}}

" vimux {{{2
map <Leader>vp :VimuxPromptCommand<Return>
map <Leader>vl :VimuxRunLastCommand<Return>
map <Leader>vi :VimuxInspectRunner<Return>
map <Leader>vq :VimuxCloseRunner<Return>
map <Leader>vx :VimuxInterruptRunner<Return>
"}}}

" ycm {{{2
augroup python
  autocmd FileType python
        \ nnoremap <silent> <buffer> <leader>gg :YcmCompleter GoTo<Return> |
        \ nnoremap <silent> <buffer> <leader>gd :YcmCompleter GoToDeclaration<Return>
augroup END
augroup rust
  let g:ycm_rust_src_path = '/usr/local/src/rustc-1.11.0/src'
  autocmd FileType rust
        \ nnoremap <silent> <buffer> <leader>gg :YcmCompleter GoTo<Return> |
        \ nnoremap <silent> <buffer> <leader>gd :YcmCompleter GoToDeclaration<Return>
augroup END
"}}}

" lightline {{{2
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'fugitive', "ale" ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype' ] ],
      \ },
      \ 'tab': {
      \   'active': [ 'tabnum', 'filename', 'modified' ],
      \   'inactive': [ 'tabnum', 'filename', 'modified' ],
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"%%":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"**":&modifiable?"":"--"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}',
      \   'ale': '%{(exists("*ALEGetStatusLine") && "OK"!=ALEGetStatusLine())?ALEGetStatusLine():""}',
      \ },
      \ 'component_visible_condition': {
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())',
      \   'ale': '(exists("*ALEGetStatusLine") && "OK"!=ALEGetStatusLine())',
      \ },
      \ 'component_type': {
      \   'ale': 'error',
      \ },
      \ }
" }}}

" vim-go {{{2
let g:go_def_mapping_enabled=0
let g:go_fmt_command="goimports"
augroup go
  autocmd FileType go nnoremap <silent> <buffer> <leader>gg :GoDef<Return>
augroup END
"}}}

" vim-rooter {{{2
let g:rooter_change_directory_for_non_project_files = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
"}}}

" bufexplorer {{{2
let g:bufExplorerDisableDefaultKeyMapping=1
let g:bufExplorerFindActive=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerShowTabBuffer=1
nnoremap <silent> [Space]bb :BufExplorer<Return>
nnoremap <silent> [Space]bs :BufExplorerHorizontalSplit<Return>
nnoremap <silent> [Space]bv :BufExplorerVerticalSplit<Return>
"}}}

" filebeagle {{{2
let g:filebeagle_suppress_keymaps=1
let g:filebeagle_check_gitignore=1
map <silent> - <Plug>FileBeagleOpenCurrentBufferDir
"}}}

" rust.vim {{{2
let g:rustfmt_autosave = 1
"}}}

"}}}

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
