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

" display line numbers
set number

" always show the status bar
set laststatus=2

" highlight matches to a search; use Esc-u to hide highlighting
set hlsearch
nnoremap <Esc>u :nohlsearch<cr>

" show tabs and trailing whitespace
set listchars=tab:»\ ,trail:·
set list

" use 256 terminal colors
set t_Co=256

" set colorscheme
colorscheme Tomorrow-Night

" gui settings
if has("gui_running")
    set guioptions+=c
    colorscheme Tomorrow
endif

" fix <s-tab>
set t_kB=[Z

" completion settings (disable scratch window)
set completeopt=menu,menuone,longest

" set cwd to that of the current file
autocmd BufEnter * silent! lcd %:p:h

" use standard indentation in python files
autocmd FileType python setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4

" use 4 space tabs in go files
autocmd FileType go setlocal tabstop=4 shiftwidth=4 nolist

" automatically run gofmt on save
autocmd BufWritePre *.go :silent Fmt

" disable netrw's banner
let g:netrw_banner=0

" contextual tab completion w/ supertab
let g:SuperTabDefaultCompletionType="context"
let g:SuperTabContextDefaultCompletionType="<tab>"

" use unicode to emulate drawing characters in powerline's status bar
let g:Powerline_symbols="unicode"
