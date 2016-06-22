call plug#begin('~/.vim/bundle')

function! BuildYCM(info)
  if a:info.status == 'installed' || a:info.force
    !./install.py
  endif
endfunction

" util {{{1
Plug 'tpope/vim-obsession'
Plug 'airblade/vim-rooter'

" integration {{{2
Plug 'tpope/vim-fugitive'
Plug 'rking/ag.vim'
"}}}
"}}}

" interface {{{1
Plug 'itchyny/lightline.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'mhinz/vim-startify'
Plug 'jlanzarotta/bufexplorer'

" colorscheme {{{2
Plug 'robertmeta/nofrils'
Plug 'nanotech/jellybeans.vim'
Plug 'jacekd/vim-iawriter'
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
"}}}

" syntax {{{2
Plug 'sheerun/vim-polyglot'
"}}}
"}}}

" file {{{1
Plug 'scrooloose/nerdtree'
Plug 'jeetsukumaran/vim-filebeagle', { 'on': '<Plug>FileBeagleOpenCurrentBufferDir' }
Plug 'kien/ctrlp.vim'
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
"}}}

" programming {{{1
Plug 'scrooloose/syntastic', { 'on': 'SyntasticCheck' }
Plug 'majutsushi/tagbar', { 'on': ['Tagbar', 'TagbarToggle'] }
Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }
Plug 'janko-m/vim-test'
Plug 'sourcegraph/sourcegraph-vim', {'for': 'go'}

" c {{{2
Plug 'WolfgangMehner/c.vim', { 'for': 'c' }
"}}}

" python {{{2
Plug 'klen/python-mode', { 'for': 'python' }
"}}}

" javascript {{{2
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
"}}}

" go {{{2
Plug 'fatih/vim-go', { 'for': 'go' }
"}}}
"}}}

call plug#end()
" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
