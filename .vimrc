set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
source ~/.vim/bundle.vim

filetype plugin indent on

" keymapping {{{
  let mapleader = ","

  " The <Space> {{{
    map <Space> [Space]

    noremap [Space] <Nop>

    nnoremap <silent> [Space]/ :nohlsearch<Return>
    nnoremap <silent> [Space]j :join<Return>
  "}}}

  " buffer {{{
    nnoremap <silent> [Space]bs :buffer #<Return>
    nnoremap <silent> [Space]bd :let w:oldbufnr=bufnr('%')<Return>
          \ :bnext<Return>
          \ :execute 'bdelete' w:oldbufnr<Return>
          \ :unlet w:oldbufnr<Return>
    nnoremap <silent> [Space]bq :bdelete<Return>
    nnoremap <silent> [Space]bh :hide<CR>
  "}}}

  " tab {{{
    nnoremap <silent>J :tabprevious<cr>
    nnoremap <silent>K :tabnext<cr>
  " }}}

  " window {{{
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l
  "}}}

  " list {{{
    nnoremap [Space]ln :lnext<Return>
    nnoremap [Space]lp :lprevious<Return>
    nnoremap [Space]ld :lclose<Return>

  " editing {{{
    inoremap jj <ESC>
  "}}}
"}}}

" general {{{
  syntax on

  if &shell =~# 'fish$'
    set shell=sh
  endif

  " encoding {{{
    set encoding=utf-8
    set fileencodings=utf-8,chinese,latin-1
  "}}}

  " editing {{{
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

  " searching {{{
    set ignorecase
    set smartcase
    set incsearch
    set hlsearch
  "}}}

  " convenience {{{
    set showmatch
    set autoread
    set wildmenu
    set wildignore+=*~,#*#
    set completeopt=menu,menuone
    " insert completion menu items
    set pumheight=15
  "}}}

  " folding {{{
    set nofoldenable
    set foldmethod=syntax
    set foldlevelstart=10
    set foldnestmax=10
  "}}}

  " backup {{{
    set backup
    set backupcopy&
    set backupdir=~/.cache/vim
    set directory=~/.cache/vim
  "}}}

  " interface {{{
    if !has('gui_running')
      set t_Co=256
    endif
    if exists('+guioptions')
      set guioptions=cgM
    endif
    set ambiwidth=double
    set noequalalways
    set ruler
    set laststatus=2
    set showtabline=1
    set number
    set showbreak='↳'
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

  " font {{{
    if exists('+guifont')
      set guifont=Hack:h15pt antialias
    endif
  "}}}

  " colorscheme {{{
    let g:zenburn_transparent = 1
    colorscheme zenburn
    set background=dark
  "}}}
"}}}

" languages {{{
  " python {{{
    autocmd FileType python
          \ setlocal colorcolumn=73,80 |
          \ setlocal iskeyword& |
          \ setlocal tabstop=4 expandtab softtabstop=4 shiftround shiftwidth=4 |
          \ setlocal smartindent cinwords=if,elseif,else,for,while,class |
          \ setlocal wildignore+=*.py[co] |
          \ setlocal foldmethod=indent |
          \ setlocal formatoptions=cq textwidth=72 foldignore=
  "}}}

  " ruby {{{
    autocmd FileType ruby setlocal tabstop=2 expandtab softtabstop=2 shiftround
  "}}}

  " c {{{
    autocmd BufRead,BufNewFile *.c,*.h set ts=4 et sw=4 sts=4
  "}}}

  " htmL {{{
    autocmd BufRead,BufNewFile *.html set ts=2 et sw=2 sts=2
  "}}}

  " php {{{
    autocmd BufRead,BufNewFile *.php set ts=4 et sw=4 sts=4
  "}}}

  " javascript {{{
    autocmd BufRead,BufNewFile *.js set ts=4 et sw=4 sts=4
    autocmd FileType javascript setlocal omnifunc=tern#Complete
    autocmd BufRead,BufNewFile *.coffee set ts=2 et sw=2 sts=2
  "}}}

  " markdown {{{
    autocmd BufRead,BufNewFile *.md,*.mdwn setlocal filetype=markdown
    autocmd FileType markdown set formatoptions=tcqnmM textwidth=79
  "}}}

  " yaml {{{
    autocmd BufRead,BufNewFile *.yaml,*.yml setlocal filetype=yaml
    autocmd FileType yaml setlocal tabstop=2 expandtab softtabstop=2 shiftround
  "}}}
"}}}

" plugin/script {{{
  " nerdtree {{{
    let NERDTreeQuitOnOpen=1
    let NERDTreeShowBookmarks=1

    map <leader>7 <ESC><ESC>:NERDTreeToggle<CR>
    map <leader>8 <ESC><ESC>:NERDTreeFind<CR>
  "}}}

  " undotree {{{
    map <leader>u :UndotreeToggle<CR>
  "}}}

  " syntastic {{{
    let g:syntastic_enable_balloons = 0
    let g:syntastic_auto_jump = 1
    let g:syntastic_python_checkers = ['flake8']
    let g:syntastic_javascript_checkers = ['jshint']
    let g:syntastic_mode_map = {
        \ "mode": "active",
        \ "active_filetypes": [],
        \ "passive_filetypes": ["python"],
    \ }
    nnoremap <silent> [Space]ec :SyntasticCheck<Return> :Errors<Return>
    nnoremap <silent> [Space]er :SyntasticReset<Return>
  "}}}

  " tagbar {{{
    map <leader>tg :TagbarToggle<cr>
  "}}}

  " ag {{{
    nnoremap <silent> <leader>a :Ag!<Return>
  "}}}

  " ctrl-p {{{
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

  " fugitive {{{
    autocmd BufReadPost fugitive://* setlocal bufhidden=delete
    nnoremap [Space]gs :Gstatus<CR>
    nnoremap [Space]gd :Gdiff<CR>
    nnoremap [Space]gc :Gcommit -v -q<CR>
    nnoremap [Space]gt :Gcommit -v -q %:p<CR>
    nnoremap [Space]gl :silent! Glog<CR>:bo copen<CR>
    nnoremap [Space]gb :Gblame<CR>
    nnoremap [Space]gB :Gbrowse<CR>
    nnoremap [Space]gv :Gitv<CR>
    nnoremap [Space]G :Git<space>
  "}}}

  " python-mode {{{
    " let g:pymode_rope = 1
    let g:pymode_rope_completion = 0
    let g:pymode_virtualenv = 1
    let g:pymode_indent = 1
    let g:pymode_breakpoint = 0
    let g:pymode_lint = 0
    let g:pymode_doc = 1
    let g:pymode_doc_bind = ''
  "}}}

  " ultisnips {{{
    let g:UltiSnipsExpandTrigger="<C-d>"
    let g:UltiSnipsJumpForwardTrigger="<C-j>"
    let g:UltiSnipsJumpBackwardTrigger="<C-k>"
    let g:UltiSnipsSnippetDirectories=["bundle/ultisnips/UltiSnips", "mysnippets"]
  "}}}

  " indent-guides {{{
    let g:indent_guides_enable_on_vim_startup = 0
    let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
    let g:indent_guides_color_change_percent = 5
  "}}}

  " vim-test {{{
    let test#strategy = "vimux"
    let test#python#runner = 'pytest'
    nmap <silent> <leader>tn :TestNearest<CR>
    nmap <silent> <leader>tf :TestFile<CR>
    nmap <silent> <leader>ta :TestSuite<CR>
    nmap <silent> <leader>tl :TestLast<CR>
    nmap <silent> <leader>tg :TestVisit<CR>
  "}}}

  " Dispatch {{{
    autocmd FileType python let b:dispatch = 'py.test %'
    nnoremap <leader>i :Dispatch<CR>
    nnoremap <leader>k :Make<space>
    nnoremap <leader>s :Dispatch<space>
  "}}}

  " vimux {{{
    map <Leader>vp :VimuxPromptCommand<CR>
    map <Leader>vl :VimuxRunLastCommand<CR>
    map <Leader>vi :VimuxInspectRunner<CR>
    map <Leader>vq :VimuxCloseRunner<CR>
    map <Leader>vx :VimuxInterruptRunner<CR>
  "}}}

  " ycm {{{
    nnoremap <silent> <leader>gg :YcmCompleter GoTo<cr>
  "}}}

  " easygrep {{{
    let g:EasyGrepRoot = 'repo'
    let g:EasyGrepCommand = 1
    set grepprg=ag\ --nocolor\ --line-numbers\ --nogroup\ -S\ $*\ /dev/null
  "}}}

  " lightline {{{
    let g:lightline = {
          \ 'active': {
          \   'left': [ [ 'mode', 'paste' ],
          \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ],
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
          \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}',
          \ },
          \ 'component_visible_condition': {
          \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
          \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())',
          \ }
          \ }
  " }}}

  " vim-go {{{
    autocmd FileType go nnoremap <silent><buffer> <leader>gg :GoDef<cr>
  "}}}
"}}}

" neovim {{{
  let g:python_host_prog = '/usr/bin/python'
" }}}

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
