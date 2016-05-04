call plug#begin('~/.vim/bundle')

function! BuildYCM(info)
  if a:info.status == 'installed' || a:info.force
    !./install.py
  endif
endfunction

" util {{{1
Plug 'tpope/vim-obsession'
Plug 'airblade/vim-rooter', { 'commit': '5e5d0553641060e07381b142172e6bd53d566621' }

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
Plug 'kols/nofrils'
Plug 'nanotech/jellybeans.vim'
Plug 'jacekd/vim-iawriter'
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
"}}}

" syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'saltstack/salt-vim'
Plug 'dag/vim-fish'
"}}}
"}}}

" file {{{1
Plug 'scrooloose/nerdtree'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'kien/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
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

" snippets {{{2
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"}}}
"}}}

" programming {{{1
Plug 'scrooloose/syntastic'
Plug 'majutsushi/tagbar'
Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }
Plug 'janko-m/vim-test'

" c {{{2
Plug 'WolfgangMehner/c.vim'
"}}}

" python {{{2
Plug 'klen/python-mode'
"}}}

" javascript {{{2
Plug 'pangloss/vim-javascript'
"}}}

" go {{{2
Plug 'fatih/vim-go'
"}}}
"}}}

call plug#end()
" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
