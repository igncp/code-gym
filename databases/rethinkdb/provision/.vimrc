execute pathogen#infect()
filetype plugin indent on
syntax on

" fix control + arrows
set term=xterm

" prevent saving backup files
set nobackup
set noswapfile

set clipboard=unnamedplus
set number
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set cursorline
set autoindent

" remove autoindentation when pasting
set pastetoggle=<F2>

let g:neocomplete#enable_at_startup = 1
let g:vim_markdown_folding_disabled = 1
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:NERDSpaceDelims = 1

let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_check_on_open = 1
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['python', 'javascript'],
                            \ 'passive_filetypes': [] }

map ,e :e <C-R>=expand("%:p:h") . "/" <CR>

nnoremap ,cd :cd %:p:h<CR>:pwd<CR>
nnoremap <Space> @d
noremap <Leader>s :update<CR>

inoremap <C-e> <Esc>A
inoremap <C-a> <Esc>I

vmap <leader>y :w! /tmp/vitmp<CR>
nmap <leader>p :r! cat /tmp/vitmp<CR>
