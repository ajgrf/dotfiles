" ~/.vimrc: initialization file for vim

" initialize pathogen
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()
syntax on
filetype plugin indent on

" backup files
set backup

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

" solarized color scheme
set t_Co=256
let g:solarized_menu=0
colorscheme solarized
set background=dark

" set cwd to that of the current file
autocmd BufEnter * silent! lcd %:p:h

" " colemak
" set langmap=sd,fe,tf,dg,ui,nj,ek,il,kn,yo,\\;p,pr,rs,gt,lu,jy,SD,FE,TF,DG,UI,NJ,EK,IL,KN,YO,:P,PR,RS,GT,LU,JY,o\\;,O:

" get control-j back, so switch it with ctrl-n at qwerty position of j
imap <c-n> <cr>
cmap <c-n> <cr>
inoremap <c-j> <c-n>
cnoremap <c-j> <c-n>

" rotate some keys about to get qwerty "hjkl" back for movement
map n <down>
map e <up>
map i <right>

" move these keys to their qwerty positions because they are
" in the way of hjkl (and E for J)
noremap k n
noremap K N
noremap u i
noremap U I
noremap l u
noremap L U
noremap N J
noremap E K
noremap I L

" this is the only key that isn't in qwerty or colemak position
noremap j e
noremap J E

" window movement
nnoremap <c-w>n <c-w>j
nnoremap <c-w>i <c-w>l
nnoremap <c-w>e <c-w>k

" qwerty <c-w>n and <c-w>i stolen but do nothing so map back
nnoremap <c-w>k <c-w>n
nnoremap <c-w>l <c-w>i

" disable netrw's banner
let g:netrw_banner=0
" open netrw's files in the second window
let g:netrw_chgwin=2
