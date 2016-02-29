call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" util {{{
  Plugin 'kien/ctrlp.vim'
  Plugin 'tacahiroy/ctrlp-funky'
  Plugin 'Raimondi/delimitMate'
  Plugin 'scrooloose/nerdtree'
  Plugin 'tpope/vim-surround'
  Plugin 'godlygeek/tabular'
  Plugin 'tpope/vim-repeat'
  " Plugin 'regedarek/ZoomWin'
  " Plugin 'bling/vim-airline'
  Plugin 'itchyny/lightline.vim'
  Plugin 'nathanaelkane/vim-indent-guides'
  Plugin 'tpope/vim-obsession'
  Plugin 'junegunn/goyo.vim'
  Plugin 'terryma/vim-multiple-cursors'
  Plugin 'mtth/scratch.vim'
  Plugin 'justinmk/vim-sneak'
  " Plugin 'tpope/vim-unimpaired'
  Plugin 'tomtom/tcomment_vim'
  Plugin 'chrisbra/NrrwRgn'
  Plugin 'mbbill/undotree'
  Plugin 'mhinz/vim-startify'
  " Plugin 'tweekmonster/braceless.vim'
  Plugin 'easymotion/vim-easymotion'
  Plugin 'airblade/vim-rooter'
"}}}

" integration {{{
  Plugin 'Shougo/vimshell.vim'
  Plugin 'rking/ag.vim'
  Plugin 'mhinz/vim-signify'
  Plugin 'tpope/vim-fugitive'
  Plugin 'gregsexton/gitv'
  " gist dep
  " Plugin 'mattn/webapi-vim'
  " Plugin 'mattn/gist-vim'
  " Plugin 'reinh/vim-makegreen'
  Plugin 'tpope/vim-dispatch'
  " Plugin 'rizzatti/dash.vim'
  Plugin 'benmills/vimux'
  Plugin 'dkprice/vim-easygrep'
"}}}

" snippets {{{
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'
"}}}

" colorscheme {{{
  Plugin 'nanotech/jellybeans.vim'
  Plugin 'w0ng/vim-hybrid'
  Plugin 'altercation/vim-colors-solarized'
  Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
  Plugin 'tomasr/molokai'
  Plugin 'jacekd/vim-iawriter'
  Plugin 'morhetz/gruvbox'
  Plugin 'jnurmine/Zenburn'
"}}}

" syntax {{{
  Plugin 'plasticboy/vim-markdown'
  " Plugin 'vim-scripts/VST'
  " Plugin 'timcharper/textile.vim'
  Plugin 'chase/vim-ansible-yaml'
  Plugin 'solarnz/thrift.vim'
  Plugin 'saltstack/salt-vim'
  Plugin 'dag/vim-fish'
"}}}

" programming {{{
  Plugin 'scrooloose/syntastic'
  Plugin 'majutsushi/tagbar'
  " Plugin 'mattn/emmet-vim'
  Plugin 'Valloric/YouCompleteMe'
  Plugin 'janko-m/vim-test'

  " c {{{
    " Plugin 'WolfgangMehner/c.vim'
  "}}}

  " ruby {{{
    " Plugin 'vim-ruby/vim-ruby'
    " Plugin 'tpope/vim-rails'
  "}}}

  " python {{{
    Plugin 'klen/python-mode'
  "}}}

  " javascript {{{
    " Plugin 'jelera/vim-javascript-syntax'
    Plugin 'pangloss/vim-javascript'
    " Plugin 'marijnh/tern_for_vim'
    " Plugin 'moll/vim-node'
    " Plugin 'nono/vim-handlebars'
  "}}}

  " coffeescript {{{
    " Plugin 'kchmck/vim-coffee-script'
  "}}}

  " go {{{
    " Plugin 'jnwhiteh/vim-golang'
    Plugin 'fatih/vim-go'
  "}}}
"}}}

call vundle#end()

" vim:fdm=marker:ts=2:sts=2:sw=2:fdl=0
