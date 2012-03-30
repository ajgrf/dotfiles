" ~/.vimrc: initialization file for vim

" initialize pathogen
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()
syntax on
filetype plugin indent on

" disable "splash" screen
set shortmess+=I

" enable mouse support
set mouse=a

" resume last cursor position
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" case-insensitive search
set ignorecase
set smartcase

" highlight matches to a search; use Esc-u to hide highlighting
set hlsearch
nnoremap <Esc>u :nohlsearch<cr>

" show tabs and trailing whitespace
set listchars=tab:»\ ,trail:·
set list

" only use terminal colors
set t_Co=16

" set cwd to that of the current file
autocmd BufEnter * silent! lcd %:p:h

" use standard indentation in python files
autocmd FileType python setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4

" automatically run gofmt on save
autocmd BufWritePre *.go :silent Fmt

" disable netrw's banner
let g:netrw_banner=0
" open netrw's files in the second window
let g:netrw_chgwin=2
