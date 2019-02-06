noremap n j
noremap e k
noremap y h
noremap o l
noremap j y
noremap k n
noremap h e
noremap l o

noremap N J
noremap E K
noremap Y H
noremap O L
noremap J Y
noremap K N
noremap H E
noremap L O

noremap gn gj
noremap ge gk
noremap gy gh
noremap gk gn
noremap gh ge
noremap gl go

noremap gN gJ
noremap gY gH
noremap gK gN
noremap gH gE
noremap gL gO

nnoremap <silent> <c-w>n :wincmd j<cr>
nnoremap <silent> <c-w>e :wincmd k<cr>
nnoremap <silent> <c-w>y :wincmd h<cr>
nnoremap <silent> <c-w>o :wincmd l<cr>
nnoremap <silent> <c-w>k :wincmd n<cr>
nnoremap <silent> <c-w>l :wincmd o<cr>

" get control-j back, so switch it with ctrl-n at qwerty position of j
imap <c-n> <cr>
cmap <c-n> <cr>
inoremap <c-j> <c-n>
cnoremap <c-j> <c-n>

nnoremap jj yy
nunmap yy
