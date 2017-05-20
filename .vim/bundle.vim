call plug#begin('~/.vim/bundle')

function! BuildYCM(info)
  if a:info.status == 'installed' || a:info.force
    !./install.py
  endif
endfunction

" util {{{1
Plug 'tpope/vim-obsession'
Plug 'airblade/vim-rooter'
Plug 'skywind3000/asyncrun.vim'

" integration {{{2
Plug 'tpope/vim-fugitive'
Plug 'rking/ag.vim'
Plug 'mhinz/vim-grepper'
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
"}}}
"}}}

" interface {{{1
Plug 'itchyny/lightline.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'mhinz/vim-startify'
Plug 'jlanzarotta/bufexplorer'

" colorscheme {{{2
Plug 'vim-scripts/peaksea'
Plug 'junegunn/seoul256.vim'
Plug 'robertmeta/nofrils'
Plug 'nanotech/jellybeans.vim'
Plug 'jacekd/vim-iawriter'
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
"}}}

" syntax {{{2
Plug 'saltstack/salt-vim'
Plug 'plasticboy/vim-markdown'
Plug 'solarnz/thrift.vim'
Plug 'pearofducks/ansible-vim'
Plug 'elzr/vim-json'
Plug 'sheerun/vim-polyglot'
"}}}
"}}}

" file {{{1
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }
Plug 'jeetsukumaran/vim-filebeagle', { 'on': '<Plug>FileBeagleOpenCurrentBufferDir' }
Plug 'ctrlpvim/ctrlp.vim', { 'on': ['CtrlP', 'CtrlPBufTag', 'CtrlPTag'] }
"}}}

" editing {{{1
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'godlygeek/tabular'
Plug 'tomtom/tcomment_vim'
Plug 'chrisbra/NrrwRgn'
Plug 'mbbill/undotree'
Plug 'easymotion/vim-easymotion'
Plug 'terryma/vim-multiple-cursors'
Plug 'Raimondi/delimitMate'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'ervandew/supertab'
"}}}

" programming {{{1
Plug 'xolox/vim-misc' | Plug 'xolox/vim-easytags'
Plug 'metakirby5/codi.vim'
Plug 'w0rp/ale' , { 'commit': 'bf8aae02e8a1a58649c4617008bd38b71b6b602d' }
Plug 'majutsushi/tagbar', { 'on': ['Tagbar', 'TagbarToggle'] }
Plug 'maralla/completor.vim'
Plug 'janko-m/vim-test'

" c {{{2
Plug 'vim-jp/vim-cpp'
"}}}

" python {{{2
Plug 'mitsuhiko/vim-python-combined'
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'lambdalisue/vim-pyenv', { 'for': 'python' }
"}}}

" javascript {{{2
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
"}}}

" go {{{2
Plug 'fatih/vim-go'
"}}}

" rust {{{2
Plug 'rust-lang/rust.vim'
"}}}
"}}}

call plug#end()
" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
