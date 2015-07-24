call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" Util {{{1
Plugin 'kien/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'Raimondi/delimitMate'
Plugin 'sjl/gundo.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'kien/tabman.vim'
Plugin 'godlygeek/tabular'
Plugin 'vim-scripts/TaskList.vim'
Plugin 'tpope/vim-repeat'
Plugin 'mikewest/vimroom'
Plugin 'vim-scripts/ZoomWin'
Plugin 'bling/vim-airline'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'tpope/vim-obsession'
Plugin 'junegunn/goyo.vim'
Plugin 'terryma/vim-multiple-cursors'

" Integration {{{1
Plugin 'rking/ag.vim'
Plugin 'mhinz/vim-signify'
Plugin 'tpope/vim-fugitive'
" gist dep
"Plugin 'mattn/webapi-vim'
"Plugin 'mattn/gist-vim'
Plugin 'tpope/vim-git'
"Plugin 'reinh/vim-makegreen'
Plugin 'tpope/vim-dispatch'
Plugin 'rizzatti/dash.vim'
Plugin 'benmills/vimux'

" Snippets {{{1
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

" Color scheme {{{1
Plugin 'nanotech/jellybeans.vim'
Plugin 'w0ng/vim-hybrid'
Plugin 'altercation/vim-colors-solarized'
Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
Plugin 'tomasr/molokai'
Plugin 'jacekd/vim-iawriter'
Plugin 'morhetz/gruvbox'
Plugin 'jnurmine/Zenburn'

" Syntax {{{1
Plugin 'tpope/vim-markdown'
"Plugin 'vim-scripts/VST'
"Plugin 'timcharper/textile.vim'
Plugin 'chase/vim-ansible-yaml'
Plugin 'solarnz/thrift.vim'

" Programming {{{1
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
"Plugin 'mattn/emmet-vim'
Plugin 'Valloric/YouCompleteMe'

    " Ruby {{{2
"Plugin 'vim-ruby/vim-ruby'
"Plugin 'tpope/vim-rails'

    " Python {{{2
Plugin 'janko-m/vim-test'
Plugin 'klen/python-mode'

    " JavaScript {{{2
"Plugin 'jelera/vim-javascript-syntax'
"Plugin 'pangloss/vim-javascript'
"Plugin 'marijnh/tern_for_vim'
"Plugin 'nono/vim-handlebars'

    " coffeescript {{{2
"Plugin 'kchmck/vim-coffee-script'

    " Go {{{2
"Plugin 'jnwhiteh/vim-golang'

call vundle#end()

" vim:fdm=marker:ts=2:sts=2:sw=2
