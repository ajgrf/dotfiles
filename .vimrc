" get off my lawn!
syntax off
set nomodeline

" use these settings only if (argv[0] != "vi")
if v:progname !=# "vi"

  " load any plugin bundles
  runtime bundle/pathogen/autoload/pathogen.vim
  silent! execute pathogen#infect()

  " enable loading plugins and indent rules based on filetype
  filetype plugin indent on

  " resume last cursor position
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  " set cwd to that of the current file
  autocmd BufEnter * silent! lcd %:p:h

  set backspace=2  " more powerful backspacing
  set mouse=a      " enable mouse support

  set completeopt=menu,menuone,longest  " disable completion scratch window
  set guioptions+=c                     " popup blocker

  " case-insensitive search
  set ignorecase smartcase
  " highlight matches to a search; use Esc-u to hide highlighting
  set hlsearch
  nnoremap <silent> <Esc>u :nohlsearch<cr>

  finish

endif

" act like traditional vi if run as vi or without eval support
set compatible
set shortmess+=I
set viminfo=
