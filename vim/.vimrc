" modelines are a possible source of vulnerabilities
set nomodeline

" use these settings only if (argv[0] != "vi")
if v:progname !=# "vi"

  " load any plugin bundles
  call plug#begin('~/.vim/bundle')
  Plug '~/src/github.com/ajgrf/parchment'
  Plug '~/src/github.com/ajgrf/sprinkles'
  Plug '~/src/github.com/ajgrf/vim-ledger'
  "Plug 'ap/vim-css-color'
  Plug 'cespare/vim-toml'
  Plug 'fatih/vim-go',       {'tag': 'v1.17'}
  Plug 'gerw/vim-HiLinkTrace'
  Plug 'tpope/vim-fugitive', {'tag': 'v2.2'}
  Plug 'tpope/vim-markdown'
  Plug 'tpope/vim-sensible', {'tag': 'v1.2'}
  Plug 'tpope/vim-surround', {'tag': 'v2.1'}
  Plug 'tpope/vim-repeat',   {'tag': 'v1.2'}
  call plug#end()

  if $TERM ==# "xterm-256color"
    set termguicolors
  endif

  " syntax highlighting settings
  syntax on
  colorscheme parchment

  " enable loading plugins and indent rules based on filetype
  filetype plugin indent on

  " resume last cursor position
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"z." | endif

  set path+=**
  set wildmenu
  set wildignorecase
  set suffixes+=.go

  set backspace=indent,eol,start
  set mouse=a      " enable mouse support
  set laststatus=1
  set autoindent
  set incsearch
  set cursorline

  autocmd VimEnter,VimResized * if &columns > 85 | set number | set laststatus=2 | else | set nonumber | set laststatus=1 | endif

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

  xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>
  function! ExecuteMacroOverVisualRange()
    echo "@".getcmdline()
    execute ":'<,'>normal @".nr2char(getchar())
  endfunction

  let g:ledger_align_at = 50
  let g:ledger_qf_reconcile_format = '%(date) %-24(payee) %-22(account) %10(amount) %10(total)\n'
  let g:ledger_qf_register_format = '%(date) %-24(payee) %-22(account) %10(amount) %10(total)\n'
  autocmd FileType ledger setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
  autocmd FileType ledger nnoremap [ ?^2<cr>nz.V/_<cr>
  autocmd FileType ledger nnoremap ] /^2<cr>z.V/_<cr>
  autocmd FileType ledger vmap [ <esc>[
  autocmd FileType ledger vmap ] <esc>]
  autocmd FileType ledger nnoremap <silent><buffer> <Leader><Space> :call ledger#transaction_state_toggle(line('.'), ' *')<CR>
  autocmd FileType ledger noremap <silent> <Leader>q :LedgerAlign<CR>

  autocmd FileType vim setlocal foldmethod=marker tabstop=8 expandtab
    \ shiftwidth=2 softtabstop=2
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 nolist
  autocmd FileType haskell setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd FileType scheme setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufRead *.json setlocal filetype=javascript expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufRead *.nix setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  autocmd FileType markdown setlocal spell
  "autocmd FileType text setlocal spell

  let g:go_fmt_command = "goimports"

  let g:plug_window = "new"

  if $TERM ==# "linux"
    colorscheme sprinkles
    set nocursorline
  endif

  finish

endif

" act like traditional vi if run as vi or without eval support
set compatible shortmess+=I viminfo=
syntax off
