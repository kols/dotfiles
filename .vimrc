set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
source ~/.vim/bundle.vim

filetype plugin indent on

" keymapping {{{
  let mapleader = ","

  " buffer {{{
    nnoremap <leader>h :hid<CR>
  "}}}

  " window {{{
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l
  "}}}

  " new-line {{{
    inoremap <C-CR> <ESC>o
  "}}}

  " editing {{{
    inoremap jj <ESC>
  "}}}
"}}}

" general {{{
  filetype plugin indent on
  syntax on

  " editing {{{
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
  "}}}

  " searching {{{
    set ignorecase " Do case insensitive matching
    set smartcase " Do smart case matching
    set incsearch " Incremental search
    set hlsearch  " highlight search matches
  "}}}

  " convenience {{{
    set autowrite " Automatically save before commands like :next and :make
    set showmatch " Show matching brackets.
    set autoread  " auto read when file changed from outside
    set wildmenu
    " set autochdir " Auto change working dir to current file
  "}}}

  " folding {{{
    set foldenable
    set foldmethod=syntax
    set ruler
  "}}}

  " backup {{{
    set nobackup " no backup file
    " set noswapfile " no more .swp file because autosave next line
  "}}}

  " interface {{{
    " set t_Co=256 " 256 color support
    set laststatus=2 " show status line
    set statusline=%F%m%r%h%w\ [F=%{&ff}]\ [T=%Y]\ [Asc=\%03.3b]\ [Hex=\%02.2B]\ [Pos=%04l,%04v][%p%%]\ [Len=%L] " content of statusline
    set statusline+=\ %{fugitive#statusline()} " add git branch after existing statusline
    set guioptions=e " Set gui interface like cli
    set showtabline=1 " show tab line when more than one tab exist
    set nu " Show line number
    set showbreak=↳ " show linebreak char
    set showcmd " Show (partial) command in status line.
    set mouse=a " Enable mouse usage (all modes)
    "set cursorline " The line cursor at is highlighted
    set splitbelow
    set splitright
    " stop flashing
    set noeb vb t_vb=
    au GUIEnter * set vb t_vb=
    set noshowmode
  "}}}

  " font {{{
    if has("macunix")
      set guifont=Hack:h15pt
    else
      set guifont=Fantasque\ Sans\ Mono\ 10
      set guifontwide=STHeiti\ 9,WenQuanYi\ Zenhei\ 9,微软雅黑\ 9,宋体\ 9
    endif
  "}}}

  " colorscheme {{{
    let g:zenburn_high_Contrast=1
    let g:zenburn_transparent = 1
    colorscheme zenburn
    set background=dark
  "}}}

  " encoding {{{
    set encoding=utf-8
    set fileencodings=utf-8,chinese,latin-1
  "}}}

  " filetype {{{
    autocmd BufRead,BufNewFile *.mdwn set ft=markdown
  "}}}
"}}}

" languages {{{
  set completeopt=menu,menuone
  " insert completion menu items
  set pumheight=15

  " python {{{
    autocmd BufRead,BufNewFile *.py set ft=python syntax=python
    autocmd BufRead,BufNewFile *.py set ai
    autocmd BufRead,BufNewFile *.py set ts=4 et sw=4 sts=4
    autocmd FileType python set isk+=.,(
    autocmd FileType python set iskeyword-=(
    autocmd FileType python set iskeyword-=.
    " Wrap at 72 chars for comments.
    set formatoptions=cq textwidth=72 foldignore= wildignore+=*.py[co] foldmethod=indent
  "}}}

  " ruby {{{
    autocmd BufRead,BufNewFile *.rb set ts=2 et sw=2 sts=2 ft=ruby
  "}}}

  " c {{{
    autocmd BufRead,BufNewFile *.c set ts=4 et sw=4 sts=4
    autocmd BufRead,BufNewFile *.h set ts=4 et sw=4 sts=4
  "}}}

  " htmL {{{
    autocmd BufRead,BufNewFile *.html set ts=2 et sw=2 sts=2
    autocmd BufRead *.py set smartindent cinwords=if,elseif,else,for,while,class
  "}}}

  " php {{{
    autocmd BufRead,BufNewFile *.php set ts=4 et sw=4 sts=4
  "}}}

  " javascript {{{
    autocmd BufRead,BufNewFile *.js set ts=2 et sw=2 sts=2
    autocmd FileType javascript setlocal omnifunc=tern#Complete
  "}}}

  " coffeescript {{{
    autocmd BufRead,BufNewFile *.coffee set ts=2 et sw=2 sts=2
  "}}}

  " markdown {{{
    autocmd FileType markdown set formatoptions=tcqnmM textwidth=79
  "}}}

  " yaml {{{
    autocmd BufRead,BufNewFile *.yml set ts=2 et sw=2 sts=2
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
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_jump = 2
    let g:syntastic_python_checkers = ['flake8']
    let g:syntastic_javascript_checkers = ['jshint']
    let g:syntastic_mode_map = {
        \ "mode": "active",
        \ "active_filetypes": [],
        \ "passive_filetypes": ["python"],
    \ }
    nnoremap <silent> <leader>el :Errors<cr>
    nnoremap <silent> <leader>es :SyntasticCheck<cr>
    nnoremap <silent> <leader>er :SyntasticReset<cr>
  "}}}

  " tagbar {{{
    map <leader>tg :TagbarToggle<cr>
  "}}}

  " ag {{{
    nmap <leader>a <Esc>:Ag!
  "}}}

  " ctrl-p {{{
    let g:ctrlp_working_path_mode = 'ra'
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
    let g:ctrlp_custom_ignore = {
      \ 'dir':  '\v[\/](node_modules|bower_components)$',
      \ }
    let g:ctrlp_persistent_input = 0
    "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    let g:ctrlp_extensions = ['funky']
    let g:ctrlp_funky_syntax_highlight = 1

    noremap <C-P> :CtrlP<CR>
    nnoremap <Leader>f :CtrlPFunky<Cr>
    " narrow the list down with a word under cursor
    nnoremap <Leader>F :execute 'CtrlPFunky ' . expand('<cword>')<Cr>)
  "}}}

  " fugitive {{{
    autocmd BufReadPost fugitive://* set bufhidden=delete
    nnoremap <leader>gs :Gstatus<CR>
    nnoremap <leader>gd :Gdiff<CR>
    nnoremap <leader>gc :Gcommit -v -q<CR>
    nnoremap <leader>gt :Gcommit -v -q %:p<CR>
    nnoremap <leader>gl :silent! Glog<CR>:bo copen<CR>
    nnoremap <leader>gb :Gblame<CR>
    nnoremap <leader>gB :Gbrowse<CR>
    nnoremap <leader>gv :Gitv<CR>
    nnoremap <leader>G :Git<space>
  "}}}

  " python-mode {{{
    let g:pymode_rope = 1
    let g:pymode_rope_completion = 0
    let g:pymode_virtualenv = 1
    let g:pymode_motion = 1
    let g:pymode_indent = 1
    let g:pymode_breakpoint = 0
    let g:pymode_folding = 0
    " use syntastic instead
    let g:pymode_lint = 0
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
    nnoremap <silent><leader>gg :YcmCompleter GoTo<cr>
  "}}}

  " unite {{{
    call unite#filters#matcher_default#use(['matcher_fuzzy'])
    call unite#filters#sorter_default#use(['sorter_rank'])
    call unite#custom#profile('default', 'context', {
    \   'start_insert': 1,
    \   'winheight': 12,
    \ })

    let g:unite_source_history_yank_enable=1

    function! s:unite_settings()
      imap <silent><buffer> <esc> <plug>(unite_exit)
      nmap <silent><buffer> <esc> <plug>(unite_exit)
      imap <silent><buffer> <C-c> <plug>(unite_exit)
      nmap <silent><buffer> <C-c> <plug>(unite_exit)
      imap <silent><buffer> <C-r> <plug>(unite_redraw)
      nmap <silent><buffer> <C-r> <plug>(unite_redraw)
    endfunction
    autocmd FileType unite call s:unite_settings()

    if executable('ag')
      let g:unite_source_rec_async_command = 'ag --nocolor --nogroup --hidden -g ""'
      let g:unite_source_grep_command = 'ag'
      let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden'
      let g:unite_source_grep_recursive_opt=''
    endif

    nmap <space> [unite]
    nnoremap [unite] <nop>

    nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
    nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=recent file_mru<cr>
    nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<cr>
    nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer<cr>
    nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
    nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
  "}}}

  " easygrep {{{
    let g:EasyGrepRoot = 'repo'
    let g:EasyGrepCommand = 1
    set grepprg=ag\ --nocolor\ --line-numbers\ --nogroup\ -S\ $*\ /dev/null
  "}}}

  " airline {{{
    let g:airline#extensions#tabline#enabled = 1
  "}}}

  " vim-go {{{
    nnoremap <silent><leader>gg :GoDef<cr>
  "}}}
"}}}

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
