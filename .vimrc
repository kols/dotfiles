set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
source ~/.vim/bundle.vim

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

" window {{{2
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"}}}

" list {{{2
nnoremap <silent> [q :cprevious<Return>
nnoremap <silent> ]q :cnext<Return>
nnoremap <silent> [l :lprevious<Return>
nnoremap <silent> ]l :lnext<Return>
"}}}

" editing {{{2
inoremap jj <Esc>
inoremap <C-e> <Esc>A
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
set backupdir=~/.cache/vim
set directory=~/.cache/vim
"}}}

" interface {{{2
if exists('+guioptions')
  set guioptions=cgM
endif
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

" https://github.com/kana/config/blob/master/vim/personal/dot.vimrc#L189
if (1 < &t_Co || has('gui')) && has('syntax')
  set t_Co=16
  if !exists('g:colors_name')
    set background=dark
    colorscheme nofrils-dark
  endif
endif
"}}}

" font {{{2
if exists('+guifont')
  set guifont=Hack:h15pt antialias
endif
"}}}
"}}}

" languages {{{1
" python {{{2
autocmd FileType python
      \ setlocal colorcolumn=+1,80 |
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
autocmd FileType javascript setlocal omnifunc=tern#Complete
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
autocmd FileType yaml
      \ setlocal tabstop=2 |
      \ setlocal expandtab |
      \ setlocal softtabstop=2 |
      \ setlocal shiftwidth=2 |
      \ setlocal shiftround
"}}}
"}}}

" plugin/script {{{1
" nerdtree {{{2
let NERDTreeQuitOnOpen=1
let NERDTreeShowBookmarks=1

map <leader>7 <ESC><ESC>:NERDTreeToggle<CR>
map <leader>8 <ESC><ESC>:NERDTreeFind<CR>
"}}}

" undotree {{{2
map <leader>u :UndotreeToggle<CR>
"}}}

" syntastic {{{2
let g:syntastic_enable_balloons = 0
let g:syntastic_auto_jump = 1
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_mode_map = {
      \ "mode": "active",
      \ "active_filetypes": [],
      \ "passive_filetypes": ["python", "go"],
      \ }
nnoremap <silent> [Space]ec :SyntasticCheck<Return> :Errors<Return>
nnoremap <silent> [Space]er :SyntasticReset<Return>
"}}}

" tagbar {{{2
map <leader>tg :TagbarToggle<cr>
"}}}

" ag {{{2
nnoremap <silent> <leader>a :Ag!<Return>
nnoremap <leader>A :Ag!<Space>
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
let g:ctrlp_extensions = ['funky']
let g:ctrlp_funky_syntax_highlight = 1

nnoremap <silent> <C-p> :CtrlP<Return>
nnoremap <silent> [Space]bb :CtrlPBuffer<Return>
nnoremap <silent> <leader>f :CtrlPFunky<Return>
"}}}

" python-mode {{{2
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_virtualenv = 1
let g:pymode_indent = 1
let g:pymode_breakpoint = 0
let g:pymode_lint = 0
let g:pymode_doc = 1
let g:pymode_doc_bind = ''
"}}}

" ultisnips {{{2
let g:UltiSnipsExpandTrigger="<C-d>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"
let g:UltiSnipsSnippetDirectories=["bundle/ultisnips/UltiSnips", "mysnippets"]
"}}}

" indent-guides {{{2
let g:indent_guides_enable_on_vim_startup = 0
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
let g:indent_guides_color_change_percent = 5
"}}}

" vim-test {{{2
let test#strategy = "vimux"
let test#python#runner = 'pytest'
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tg :TestVisit<CR>
"}}}

" Dispatch {{{2
autocmd FileType python let b:dispatch = 'py.test %'
nnoremap <leader>i :Dispatch<CR>
nnoremap <leader>k :Make<space>
nnoremap <leader>s :Dispatch<space>
"}}}

" vimux {{{2
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vi :VimuxInspectRunner<CR>
map <Leader>vq :VimuxCloseRunner<CR>
map <Leader>vx :VimuxInterruptRunner<CR>
"}}}

" ycm {{{2
nnoremap <silent> <leader>gg :YcmCompleter GoTo<cr>
"}}}

" lightline {{{2
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'syntastic', 'lineinfo' ],
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
      \ },
      \ 'component_visible_condition': {
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \ }
      \ }
" }}}

" vim-go {{{2
autocmd FileType go nnoremap <silent><buffer> <leader>gg :GoDef<cr>
let g:go_fmt_command = "goimports"
"}}}

" vim-rooter {{{2
let g:rooter_change_directory_for_non_project_files = 1
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
"}}}

" bufexplorer {{{2
let g:bufExplorerDisableDefaultKeyMapping=1
nnoremap <silent> [Space]bs :BufExplorerHorizontalSplit<Return>
nnoremap <silent> [Space]bv :BufExplorerVerticalSplit<Return>
"}}}
"}}}

" neovim {{{1
let g:python_host_prog = '/usr/bin/python'
" }}}

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
