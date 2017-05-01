" modelines are a possible source of vulnerabilities
set nomodeline

" use these settings only if (argv[0] != "vi")
if v:progname !=# "vi"

  " load any plugin bundles
  call plug#begin('~/.vim/bundle')
  Plug 'tpope/vim-fugitive', {'tag': 'v2.2'}
  Plug 'fatih/vim-go',       {'tag': 'v1.12'}
  Plug 'rust-lang/rust.vim'
  Plug 'tpope/vim-sensible', {'tag': 'v1.1'}
  Plug 'tpope/vim-eunuch',   {'tag': 'v1.1'}
  Plug 'tpope/vim-surround', {'tag': 'v2.1'}
  Plug 'tpope/vim-repeat',   {'tag': 'v1.1'}
  "Plug 'justinmk/vim-sneak', {'tag': '1.8.1'}
  Plug 'ervandew/supertab',  {'tag': '2.1'}
  Plug 'gerw/vim-HiLinkTrace'
  Plug 'terryma/vim-multiple-cursors', {'tag': 'v2.2'}
  Plug 'cespare/vim-toml'
  "Plug 'neovimhaskell/haskell-vim'
  Plug 'ledger/vim-ledger'
  call plug#end()

  " syntax highlighting settings
  syntax on
  colorscheme plan9

  " enable loading plugins and indent rules based on filetype
  filetype plugin indent on

  " resume last cursor position
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  " set cwd to that of the current file
  autocmd BufEnter * silent! lcd %:p:h

  set backspace=2  " more powerful backspacing
  set mouse=a      " enable mouse support
  set number

  set completeopt=menu,menuone,longest  " disable completion scratch window
  set guioptions+=c                     " popup blocker
  set guifont=Go\ Mono\ 11

  " case-insensitive search
  set ignorecase smartcase
  " highlight matches to a search; use Esc-u to hide highlighting
  set hlsearch
  nnoremap <silent> <Esc>u :nohlsearch<cr>

  " disable folding by default
  set nofoldenable

  autocmd FileType vim setlocal foldmethod=marker tabstop=8 expandtab
    \ shiftwidth=2 softtabstop=2
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 nolist
  autocmd FileType haskell setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd FileType ledger setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd BufNewFile,BufRead *.json setlocal filetype=javascript expandtab shiftwidth=2 softtabstop=4
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown

  set path+=~/Wiki
  set suffixesadd+=.md

  let g:go_fmt_command = "goimports"

  let g:plug_window = "new"

  finish

endif

" act like traditional vi if run as vi or without eval support
set compatible shortmess+=I viminfo=
syntax off
