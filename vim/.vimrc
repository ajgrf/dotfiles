" modelines are a possible source of vulnerabilities
set nomodeline

" use these settings only if (argv[0] != "vi")
if v:progname !=# "vi"

  " load any plugin bundles
  call plug#begin('~/.vim/bundle')
  Plug 'tpope/vim-fugitive', {'tag': 'v2.2'}
  Plug 'fatih/vim-go',       {'tag': 'v1.11'}
  Plug 'rust-lang/rust.vim'
  Plug 'tpope/vim-sensible', {'tag': 'v1.1'}
  Plug 'tpope/vim-eunuch',   {'tag': 'v1.1'}
  Plug 'tpope/vim-surround', {'tag': 'v2.1'}
  Plug 'tpope/vim-repeat',   {'tag': 'v1.1'}
  "Plug 'bling/vim-airline' ",  {'tag': 'v0.8'}
  "Plug 'justinmk/vim-sneak', {'tag': '1.8.1'}
  Plug 'ervandew/supertab',  {'tag': '2.1'}
  Plug 'gerw/vim-HiLinkTrace'
  Plug 'terryma/vim-multiple-cursors', {'tag': 'v2.2'}
  Plug 'cespare/vim-toml'
  "Plug 'neovimhaskell/haskell-vim'
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

  autocmd FileType vim setlocal foldmethod=marker tabstop=8 expandtab
    \ shiftwidth=2 softtabstop=2
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 nolist
  autocmd FileType haskell setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd BufNewFile,BufRead *.json setlocal filetype=javascript expandtab shiftwidth=2 softtabstop=4
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown

  set path+=~/Wiki
  set suffixesadd+=.md

  let g:go_fmt_command = "goimports"

  let g:plug_window = "new"

  let g:airline_theme='sprinkles'
  "let g:airline_left_sep = ''
  "let g:airline_left_alt_sep = ''
  "let g:airline_right_sep = ''
  "let g:airline_right_alt_sep = ''
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  "let g:airline_symbols.branch = ''
  "let g:airline_symbols.readonly = ''
  "let g:airline_symbols.linenr = ''

  " Adjust keybindings for the Colemak keyboard layout
  if $COLEMAK
    " basic key rearrangements
    noremap n j
    noremap e k
    noremap i l
    noremap k n
    noremap u i
    noremap l u
    noremap j e

    noremap N J
    noremap E K
    noremap I L
    noremap K N
    noremap U I
    noremap L U
    noremap J E

    " g- movement keys
    noremap gn gj
    noremap gN gJ
    noremap ge gk
    noremap gj ge
    noremap gJ gE
    unmap gk
    unmap gE

    " get control-j back, so switch it with ctrl-n at qwerty position of j
    imap <c-n> <cr>
    cmap <c-n> <cr>
    inoremap <c-j> <c-n>
    cnoremap <c-j> <c-n>

    " window movement
    nnoremap <c-w>n <c-w>j
    nnoremap <c-w>i <c-w>l
    nnoremap <c-w>e <c-w>k

    " qwerty <c-w>n and <c-w>i stolen but do nothing so map back
    nnoremap <c-w>k <c-w>n
    nnoremap <c-w>l <c-w>i
  endif

  finish

endif

" act like traditional vi if run as vi or without eval support
set compatible shortmess+=I viminfo=
syntax off
