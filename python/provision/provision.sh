#!/bin/bash

if [ -d /project/scripts ]; then chmod -R +x /project/scripts; fi

mkdir -p ~/logs

if ! type jq > /dev/null 2>&1 ; then
  echo "installing basic packages"
  sudo apt-get update
  sudo apt-get install -y curl git unzip git-extras \
    build-essential python-software-properties tree entr jq

  git config --global user.email "foo@bar.com" && git config --global user.name "Foo Bar"
fi

if [ ! -d ~/src ]; then cp -r /project/src ~; fi

cat > ~/.bashrc <<"EOF"
# move from word to word. avoid ctrl+b to use in tmux
  bind '"\C-g":vi-fWord' > /dev/null 2>&1
  bind '"\C-f":vi-bWord' > /dev/null 2>&1

stty -ixon # prevent the terminal from hanging on ctrl+s

export HISTCONTROL=ignoreboth:erasedups

source_if_exists() {
  FILE_PATH=$1
  if [ -f $FILE_PATH ]; then source $FILE_PATH; fi
}

source_if_exists ~/.bash_aliases

if [[ -z $TMUX ]]; then TMUX_PREFIX="·"; else TMUX_PREFIX="£"; fi
PS1='${debian_chroot:+($debian_chroot)}\n\u@\h: \W$(__git_ps1) $TMUX_PREFIX '

alias ll="ls -lah"
alias rm="rm -rf"
alias mkdir="mkdir -p"
alias cp="cp -r"
alias tmux="tmux; exit"
alias Exit="killall tmux"

Update_src() {
  rm -rf /project/src
  rsync -av \
    --exclude='*node_modules*' \
    ~/src /project
}

export PATH=$PATH:/project/scripts
export PATH=$PATH:/project/provision
export PATH=$PATH:~/.local/bin

if [ -d ~/src ]; then cd ~/src; fi
EOF

cat > ~/.tmux.conf <<"EOF"
set -g status off
set-window-option -g xterm-keys on

bind-key S-Left swap-window -t -1
bind-key S-Right swap-window -t +1
EOF

if ! type pip > /dev/null  ; then
  echo "installing python tools"

  sudo apt-get install -y python-pip
fi

GLOBAL_PIP_MODULES=(flake8 discogs_client wikipedia)

for MODULE_NAME in "${GLOBAL_PIP_MODULES[@]}"; do
  if [ ! -d /usr/local/lib/python2.7/dist-packages/$MODULE_NAME ]; then
    echo "doing: sudo pip install $MODULE_NAME"
    sudo pip install $MODULE_NAME
  fi
done

install_vim_package() {
  REPO=$1
  DIR=$(echo $REPO | sed -r "s|.+/(.+)|\1|") # foo/bar => bar
  EXTRA_CMD=$2
  if [ ! -d ~/.vim/bundle/"$DIR" ]; then
    git clone https://github.com/$REPO.git ~/.vim/bundle/"$DIR"
    if [[ ! -z $EXTRA_CMD ]]; then eval $EXTRA_CMD; fi
  fi
}

mkdir -p ~/.vim/autoload/ ~/.vim/bundle
if [ ! -f ~/.vim/autoload/pathogen.vim ]; then
  curl https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim \
    > ~/.vim/autoload/pathogen.vim
fi

install_vim_package airblade/vim-gitgutter
install_vim_package ctrlpvim/ctrlp.vim
install_vim_package elzr/vim-json
install_vim_package evidens/vim-twig
install_vim_package honza/vim-snippets
install_vim_package jiangmiao/auto-pairs
install_vim_package milkypostman/vim-togglelist
install_vim_package nathanaelkane/vim-indent-guides
install_vim_package ntpeters/vim-better-whitespace
install_vim_package plasticboy/vim-markdown
install_vim_package scrooloose/nerdcommenter
install_vim_package scrooloose/syntastic
install_vim_package shougo/neocomplete.vim "sudo apt-get install -y vim-nox"
install_vim_package shougo/neosnippet.vim
install_vim_package shougo/vimproc.vim "cd ~/.vim/bundle/vimproc.vim && make; cd -"
install_vim_package vim-airline/vim-airline
install_vim_package vim-airline/vim-airline-themes
install_vim_package vim-scripts/cream-showinvisibles
# python
  install_vim_package nvie/vim-flake8

cat > ~/.vimrc <<"EOF"
execute pathogen#infect()
filetype plugin indent on
syntax on
set background=dark

" fix control + arrows
  set term=xterm

" prevent saving backup files
  set nobackup
  set noswapfile

" support all hex colors (e.g. for syntastic)
  set  t_Co=256

" move lines up and down
  nnoremap <C-j> :m .+1<CR>==
  nnoremap <C-k> :m .-2<CR>==
  inoremap <C-j> <Esc>:m .+1<CR>==gi
  inoremap <C-k> <Esc>:m .-2<CR>==gi
  vnoremap <C-j> :m '>+1<CR>gv=gv
  vnoremap <C-k> :m '<-2<CR>gv=gv

" remove trailing spaces
  nmap <leader>t :%s/\s\+$<CR><C-o>

set autoindent
set clipboard=unnamedplus
set cursorline
set expandtab
set number
set shiftwidth=2
set softtabstop=2
set tabstop=2

" airline
  set laststatus=2

" remove autoindentation when pasting
  set pastetoggle=<F2>

" neocomplete
  let g:neocomplete#enable_at_startup = 1

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 0
let g:NERDSpaceDelims = 1

" ctrlp
  let g:ctrlp_map = '<c-p>'
  let g:ctrlp_cmd = 'CtrlP'
  let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'
  nnoremap <leader>p :CtrlP %:p:h<CR> " CtrlP in file's dir

" syntastic
  let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['python', 'javascript'],
                            \ 'passive_filetypes': [] }
  set statusline+=%#warningmsg#
  set statusline+=%{SyntasticStatuslineFlag()}
  set statusline+=%*
  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_check_on_wq = 0
  let g:syntastic_javascript_checkers = ['eslint']
  let g:syntastic_typescript_checkers = ['tsc', 'tslint']
  let g:syntastic_json_checkers=[]
  highlight link SyntasticErrorSign SignColumn
  highlight link SyntasticWarningSign SignColumn
  highlight link SyntasticStyleErrorSign SignColumn
  highlight link SyntasticStyleWarningSign SignColumn
  let g:syntastic_error_symbol = '❌'
  let g:syntastic_style_error_symbol = '⁉️'
  hi Error ctermbg=lightred ctermfg=black
  hi SpellBad ctermbg=lightred ctermfg=black

map ,e :e <C-R>=expand("%:p:h") . "/" <CR>

" move up/down from the beginning/end of lines
  set ww+=<,>

" change to current file directory
  nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

" run macro on d
  nnoremap <Space> @d

" sort lines
  vmap <F5> :sort<CR>

inoremap <C-e> <Esc>A
inoremap <C-a> <Esc>I

" neosnippet
  imap <C-l>     <Plug>(neosnippet_expand_or_jump)
  smap <C-l>     <Plug>(neosnippet_expand_or_jump)
  xmap <C-l>     <Plug>(neosnippet_expand_target)
  imap <expr><TAB>
   \ pumvisible() ? "\<C-n>" :
   \ neosnippet#expandable_or_jumpable() ?
   \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
   \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  if has('conceal')
    set conceallevel=2 concealcursor=niv
  endif
  let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
  let g:neosnippet#disable_runtime_snippets={'c' : 1, 'cpp' : 1,}

" save file shortcuts
  nmap <C-s> :update<Esc>
  inoremap <C-s> <Esc>:update<Esc>i<right>

" copy - paste between files
  vmap <leader>ky :w! /tmp/vitmp<CR>
  nmap <leader>kp :r! cat /tmp/vitmp<CR>

" improve the 'preview window' behaviour
  autocmd CompleteDone * pclose " close when done
  set splitbelow " move to the bottom

" python (flake8)
  let g:flake8_show_quickfix=0  " don't show quickfix
EOF
