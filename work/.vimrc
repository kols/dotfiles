set nocompatible

filetype plugin indent on

" keymapping {{{1
let mapleader = ","

" The <Space> {{{2
map <Space> [Space]

noremap [Space] <Nop>

nnoremap <silent> [Space]/ :nohlsearch<Return>
nnoremap <silent> [Space]j :join<Return>
vnoremap [Space]so y:execute @@<Return>:echo 'selection evaluated'<Return>
nnoremap [Space]so yy:execute @@<Return>:echo 'line evaluated'<Return>
"}}}

" buffer {{{2
nnoremap <silent> [Space]bs :buffer #<Return>
nnoremap <silent> [Space]bd :let w:oldbufnr=bufnr('%')<Return>
      \ :bnext<Return>
      \ :execute 'bdelete' w:oldbufnr<Return>
      \ :unlet w:oldbufnr<Return>
nnoremap <silent> [Space]bq :bdelete<Return>
nnoremap <silent> [Space]bh :hide<CR>
"}}}

" tab {{{2
nnoremap <silent>J :tabprevious<cr>
nnoremap <silent>K :tabnext<cr>
" }}}

" window {{{2
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"}}}

" list {{{2
nnoremap [Space]ln :lnext<Return>
nnoremap [Space]lp :lprevious<Return>
nnoremap [Space]ld :lclose<Return>
" }}}

" editing {{{2
inoremap jj <Esc>
inoremap aa <Esc>A
"}}}
"}}}

" general {{{1
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
"}}}

" interface {{{2
set ambiwidth=double
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
"}}}
"}}}

" languages {{{1
" python {{{2
autocmd FileType python
      \ setlocal colorcolumn=73,80 |
      \ setlocal iskeyword& |
      \ setlocal tabstop=4 expandtab softtabstop=4 shiftround shiftwidth=4 |
      \ setlocal smartindent cinwords=if,elseif,else,for,while,class |
      \ setlocal wildignore+=*.py[co] |
      \ setlocal foldmethod=indent |
      \ setlocal formatoptions=cq textwidth=72 foldignore=
"}}}

" go {{{2
autocmd FileType go setlocal colorcolumn=
" }}}

" ruby {{{2
autocmd FileType ruby setlocal tabstop=2 expandtab softtabstop=2 shiftround
"}}}

" c {{{2
autocmd BufRead,BufNewFile *.c,*.h set ts=4 et sw=4 sts=4
"}}}

" html {{{2
autocmd BufRead,BufNewFile *.html set ts=2 et sw=2 sts=2
"}}}

" php {{{2
autocmd BufRead,BufNewFile *.php set ts=4 et sw=4 sts=4
"}}}

" javascript {{{2
autocmd BufRead,BufNewFile *.js set ts=4 et sw=4 sts=4
autocmd BufRead,BufNewFile *.coffee set ts=2 et sw=2 sts=2
"}}}

" markdown {{{2
autocmd BufRead,BufNewFile *.md,*.mdwn setlocal filetype=markdown
autocmd FileType markdown
      \ setlocal formatoptions=tcqnmM |
      \ setlocal textwidth=79 |
      \ setlocal conceallevel=2
"}}}

" yaml {{{2
autocmd BufRead,BufNewFile *.yaml,*.yml setlocal filetype=yaml
autocmd FileType yaml setlocal tabstop=2 expandtab softtabstop=2 shiftround
"}}}
"}}}

" neovim {{{1
let g:python_host_prog = '/usr/bin/python'
" }}}

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
