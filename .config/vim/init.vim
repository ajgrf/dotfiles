" modelines are a possible source of vulnerabilities
set nomodeline

" use these settings only if (argv[0] != 'vi')
if v:progname !=# 'vi'

  " file locations
  if !has('nvim')
    set runtimepath-=~/.vim
    set runtimepath-=~/.vim/after
    set runtimepath^=$XDG_CONFIG_HOME/vim
    set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
  endif
  " disable .netrwhist file
  let g:netrw_dirhistmax = 0

  " load any plugin bundles
  call plug#begin("$XDG_DATA_HOME/vim/bundle")
  Plug '~/src/parchment'
  Plug '~/src/sprinkles'
  call plug#end()

  " open vim-plug in new window
  let g:plug_window = 'new'

  " set the xterm title
  set title

  " syntax highlighting settings
  syntax on
  if $TERM ==# 'linux'
    colorscheme sprinkles
  else
    colorscheme parchment
    set cursorline
  endif

  " enable loading plugins and indent rules based on filetype
  filetype plugin indent on

  " resume last cursor position
  autocmd BufReadPost * if line('''"') > 1 && line('''"') <= line('$') | exe 'normal! g''"z.' | endif

  set path+=**
  set wildmenu
  set wildignorecase

  set backspace=indent,eol,start
  set mouse=a      " enable mouse support
  set laststatus=2
  set autoindent
  set incsearch

  autocmd VimEnter,VimResized * if &columns >= 85 | set number | else | set nonumber | endif

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
    echo '@'.getcmdline()
    execute ':''<,''>normal @'.nr2char(getchar())
  endfunction

  autocmd FileType vim setlocal foldmethod=marker tabstop=8 expandtab
    \ shiftwidth=2 softtabstop=2
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 nolist
  autocmd FileType haskell setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd FileType scheme setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufRead *.json setlocal filetype=javascript expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufRead *.nix setlocal tabstop=8 expandtab shiftwidth=2 softtabstop=2
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  autocmd FileType markdown setlocal spell linebreak

  " use workman bindings if found
  if $WORKMAN
    source $XDG_CONFIG_HOME/vim/workman.vim
  endif

  finish

endif

" act like traditional vi if run as vi or without eval support
set compatible shortmess+=I viminfo=
syntax off
