" Load vundle bundles
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
source ~/.vim/bundle.vim

filetype plugin indent on

" Keymapping {{{1
let mapleader = ","

  " Buffer {{{2
nnoremap <leader>q :bd<CR>
nnoremap <leader>h :hid<CR>

  " move between splited window {{{2
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

  " new line anywhere {{{2
inoremap <C-CR> <ESC>o

  " tab operations {{{2
" {Alt + Shift + ]} Switch to the next tab to the right
noremap <A-}> :tabnext<CR>
noremap! <A-}> <ESC>:tabnext<CR>i
" {Alt + Shift + [} Switch to the next tab to the left
noremap <A-{> :tabprevious<CR>
noremap! <A-{> <ESC>:tabprevious<CR>i
" {Ctrl + Alt + t} Create a new tab
noremap <C-A-t> :tabnew<CR>
noremap! <C-A-t> <ESC>:tabnew<CR>
" {Ctrl + Alt + w} Close current tab
noremap <C-A-w> :tabclose<CR>
noremap! <C-A-w> <ESC>:tabclose<CR>
" One tab for every opened buffer
noremap <C-A-b> :tab sball<CR>
noremap! <C-A-b> <ESC>:tab sball<CR>

  " location list movement {{{2
nnoremap ]l :lne<CR>
nnoremap [l :lp<CR>

  " editing {{{2
inoremap jj <ESC>

" General {{{1
filetype plugin indent on
syntax on

  " editing {{{2
" jump to the last position when reopening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
set wrap " Warp the over-length lines
set textwidth=79 " auto newline at specified column
set colorcolumn=72,80 " a vertical bar indicates line length
set backspace=indent,eol,start " <backspace> acts normally
set autoindent " Turn on autoindent
set tabstop=4 " number of spaces a <tab> in file counts for
set expandtab " use space to insert a <tab> and >, <
set shiftwidth=4 " number of spaces for each step of (auto)indent,
                 " for cindent, >>, <<, etc.
set softtabstop=4
set shiftround
set modeline " Make enable per file vim options
set hidden " Hide buffers when they are abandoned
autocmd BufWritePre [^(mutt)]* :%s/\s\+$//e " auto remove trailing space

" Highlight end of line whitespace.
highlight WhitespaceEOL ctermbg=red guibg=red
match WhitespaceEOL /\s\+$/

  " searching {{{2
set ignorecase " Do case insensitive matching
set smartcase " Do smart case matching
set incsearch " Incremental search
set hlsearch  " highlight search matches

  " convenience {{{2
set autowrite " Automatically save before commands like :next and :make
set showmatch " Show matching brackets.
set autoread  " auto read when file changed from outside
set wildmenu
"set autochdir " Auto change working dir to current file

  " folding {{{2
set foldenable
set foldmethod=syntax
set ruler

  " backup {{{2
set nobackup " no backup file
"set noswapfile " no more .swp file because autosave next line

  " interface {{{2
"set t_Co=256 " 256 color support
set laststatus=2 " show status line
set statusline=%F%m%r%h%w\ [F=%{&ff}]\ [T=%Y]\ [Asc=\%03.3b]\ [Hex=\%02.2B]\ [Pos=%04l,%04v][%p%%]\ [Len=%L] " content of statusline
set statusline+=\ %{fugitive#statusline()} " add git branch after existing statusline
set guioptions=e " Set gui interface like cli
set showtabline=1 " show tab line when more than one tab exist
set nu " Show line number
set list " show list chars set below
set listchars=tab:▸\ ,eol:¬
set showbreak=↳ " show linebreak char
set showcmd " Show (partial) command in status line.
set mouse=a " Enable mouse usage (all modes)
"set cursorline " The line cursor at is highlighted
set splitbelow
set splitright
" stop flashing
set noeb vb t_vb=
au GUIEnter * set vb t_vb=

  " font {{{2
if has("macunix")
  set guifont=Fantasque\ Sans\ Mono:h16pt
else
  set guifont=Fantasque\ Sans\ Mono\ 10
  set guifontwide=STHeiti\ 9,WenQuanYi\ Zenhei\ 9,微软雅黑\ 9,宋体\ 9
endif

  " colorscheme {{{2
let g:zenburn_high_Contrast=1
colorscheme zenburn
set background=dark

  " Encoding {{{2
set encoding=utf-8
set fileencodings=utf-8,chinese,latin-1

  " view pdf {{{2
autocmd BufReadPre *.pdf set ro nowrap
autocmd BufReadPost *.pdf silent %!pdftotext "%" -nopgbrk -layout -q -eol unix -
autocmd BufWritePost *.pdf silent !rm -rf ~/PDF/%
autocmd BufWritePost *.pdf silent !lp -s -d pdffg "%"
autocmd BufWritePost *.pdf silent !until [ -e ~/PDF/% ]; do sleep 1; done
autocmd BufWritePost *.pdf silent !mv ~/PDF/% %:p:h

  " filetype {{{2
autocmd BufRead,BufNewFile *.mdwn set ft=markdown

" Languages {{{1
" " General {{{2
set completeopt=menu,menuone
" insert completion menu items
set pumheight=15

  " Python {{{2
autocmd BufRead,BufNewFile *.py set ft=python syntax=python
autocmd BufRead,BufNewFile *.py set ai
autocmd BufRead,BufNewFile *.py set ts=4 et sw=4 sts=4
autocmd FileType python set isk+=.,(
autocmd FileType python set iskeyword-=(
autocmd FileType python set iskeyword-=.
" Wrap at 72 chars for comments.
set formatoptions=cq textwidth=72 foldignore= wildignore+=*.py[co]

  " Ruby {{{2
autocmd BufRead,BufNewFile *.rb set ts=2 et sw=2 sts=2 ft=ruby

  " C {{{2
autocmd BufRead,BufNewFile *.c set ts=4 et sw=4 sts=4
autocmd BufRead,BufNewFile *.h set ts=4 et sw=4 sts=4

  " HTML {{{2
autocmd BufRead,BufNewFile *.html set ts=2 et sw=2 sts=2
autocmd BufRead *.py set smartindent cinwords=if,elseif,else,for,while,class

  " PHP {{{2
autocmd BufRead,BufNewFile *.php set ts=4 et sw=4 sts=4

  " JavaScript {{{2
autocmd BufRead,BufNewFile *.js set ts=2 et sw=2 sts=2
autocmd BufRead,BufNewFile jquery.*.js set ft=javascript syntax=jquery
autocmd FileType javascript set foldmethod=indent

  " CoffeeScript {{{2
autocmd BufRead,BufNewFile *.coffee set ts=2 et sw=2 sts=2

  " Markdown {{{2
autocmd FileType markdown set formatoptions=tcqnmM textwidth=79

  " thrift {{{2
au BufRead,BufNewFile *.thrift set filetype=thrift

  " yaml {{{2
autocmd BufRead,BufNewFile *.yml set ts=2 et sw=2 sts=2

" Plugin/script {{{1
  " NERDTree {{{2
let NERDTreeQuitOnOpen=1
let NERDTreeShowBookmarks=1

map <leader>7 <ESC><ESC>:NERDTreeToggle<CR>
map <leader>8 <ESC><ESC>:NERDTreeFind<CR>

  " Tasklist {{{2
map <leader>td <Plug>TaskList

  " Gundo {{{2
map <leader>g :GundoToggle<CR>

  " Syntastic {{{2
let g:syntastic_enable_balloons = 0
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_jump = 2
let g:syntastic_python_checkers = ['flake8', 'pylint']
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_mode_map = {
    \ "mode": "active",
    \ "active_filetypes": [],
    \ "passive_filetypes": ["python"],
\ }
nnoremap <silent> <leader>el :Errors<cr>
nnoremap <silent> <leader>es :SyntasticCheck<cr>

  " tagbar {{{2
map <leader>tg :TagbarToggle<cr>

  " ag {{{2
nmap <leader>a <Esc>:Ag!

  " ctrl-p {{{2
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](node_modules|bower_components)$',
  \ }
let g:ctrlp_persistent_input = 0
"let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_extensions = ['funky']
let g:ctrlp_funky_syntax_highlight = 1

noremap <leader>b :CtrlPBuffer<CR>
noremap! <leader>b <ESC>:CtrlPBuffer<CR>
noremap <C-P> :CtrlP<CR>
nnoremap <Leader>f :CtrlPFunky<Cr>
" narrow the list down with a word under cursor
nnoremap <Leader>F :execute 'CtrlPFunky ' . expand('<cword>')<Cr>)

  " fugitive {{{2
autocmd BufReadPost fugitive://* set bufhidden=delete
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gc :Gcommit -v -q<CR>
nnoremap <leader>gt :Gcommit -v -q %:p<CR>
nnoremap <leader>gl :silent! Glog<CR>:bo copen<CR>
nnoremap <leader>gb :Gbrowse<CR>
nnoremap <leader>grv :Gtabedit<space>

  " python-mode {{{2
let g:pymode_rope = 1
let g:pymode_rope_completion = 0
let g:pymode_virtualenv = 1
let g:pymode_motion = 1
let g:pymode_indent = 1
let g:pymode_breakpoint = 0
" use syntastic instead
let g:pymode_lint = 0
let g:pymode_rope_goto_definition_bind = '<leader>gg'

  " UltiSnips {{{2
let g:UltiSnipsExpandTrigger="<C-d>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"
let g:UltiSnipsSnippetDirectories=["bundle/ultisnips/UltiSnips", "mysnippets"]

  " indent-guides {{{2
let g:indent_guides_enable_on_vim_startup = 0
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
let g:indent_guides_color_change_percent = 5

  " vim-test {{{2
let test#strategy = "vimux"
let test#python#runner = 'pytest'
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tg :TestVisit<CR>

  " Dispatch {{{2
autocmd FileType python let b:dispatch = 'py.test %'
nnoremap <leader>i :Dispatch<CR>

  " vimux {{{2
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vi :VimuxInspectRunner<CR>
map <Leader>vq :VimuxCloseRunner<CR>
map <Leader>vx :VimuxInterruptRunner<CR>

" Modeline {{{1
" vim:fdm=marker:ts=2:sts=2:sw=2
