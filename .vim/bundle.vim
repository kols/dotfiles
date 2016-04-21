call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" util {{{1
Plugin 'tpope/vim-obsession'
Plugin 'airblade/vim-rooter'

" integration {{{2
Plugin 'tpope/vim-fugitive'
Plugin 'rking/ag.vim'
"}}}
"}}}

" interface {{{1
Plugin 'itchyny/lightline.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'mhinz/vim-startify'
Plugin 'jlanzarotta/bufexplorer'

" colorscheme {{{2
Plugin 'kols/nofrils'
Plugin 'nanotech/jellybeans.vim'
Plugin 'jacekd/vim-iawriter'
Plugin 'morhetz/gruvbox'
Plugin 'jnurmine/Zenburn'
"}}}

" syntax {{{2
Plugin 'sheerun/vim-polyglot'
Plugin 'saltstack/salt-vim'
Plugin 'dag/vim-fish'
"}}}
"}}}

" file {{{1
Plugin 'scrooloose/nerdtree'
Plugin 'jeetsukumaran/vim-filebeagle'
Plugin 'kien/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'
"}}}

" editing {{{1
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'godlygeek/tabular'
Plugin 'tomtom/tcomment_vim'
Plugin 'chrisbra/NrrwRgn'
Plugin 'mbbill/undotree'
Plugin 'easymotion/vim-easymotion'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'Raimondi/delimitMate'

" snippets {{{2
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
"}}}
"}}}

" programming {{{1
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'Valloric/YouCompleteMe'
Plugin 'janko-m/vim-test'

" c {{{2
Plugin 'WolfgangMehner/c.vim'
"}}}

" python {{{2
Plugin 'klen/python-mode'
"}}}

" javascript {{{2
Plugin 'pangloss/vim-javascript'
"}}}

" go {{{2
Plugin 'fatih/vim-go'
"}}}
"}}}

call vundle#end()
" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
