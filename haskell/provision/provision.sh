#!/usr/bin/env bash

chmod -R +x /project/scripts

mkdir -p ~/logs

if ! type jq > /dev/null 2>&1 ; then
  echo "installing basic packages"
  sudo apt-get update
  sudo apt-get install -y curl git unzip ack-grep git-extras \
    build-essential python-software-properties tree jq

  git config --global user.email "foo@bar.com" && git config --global user.name "Foo Bar"
fi

if [ ! -d ~/src ]; then cp -r /project/src ~; fi

cat > ~/.bashrc <<"EOF"
# move from word to word. avoid ctrl+b to use in tmux
  bind '"\C-g":vi-fWord' > /dev/null 2>&1
  bind '"\C-f":vi-bWord' > /dev/null 2>&1

stty -ixon # prevent the terminal from hanging on ctrl+s

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

export PATH=$PATH:/project/scripts
export PATH=$PATH:/project/provision
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.cabal/bin

# haskell
  alias runghc="stack exec runghc --silent -- -w -ihs"
  alias vim="stack exec vim"

if [ -d ~/src ]; then cd ~/src; fi
EOF

cat > ~/.tmux.conf <<"EOF"
set -g status off
set-window-option -g xterm-keys on

bind-key S-Left swap-window -t -1
bind-key S-Right swap-window -t +1
EOF

if ! type stack > /dev/null 2>&1 ; then
  echo "installing haskell"
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list && \
    sudo apt-get update && sudo apt-get install stack -y && \
    stack setup && \
    stack upgrade --git
fi

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
install_vim_package jiangmiao/auto-pairs
install_vim_package milkypostman/vim-togglelist
install_vim_package nathanaelkane/vim-indent-guides
install_vim_package ntpeters/vim-better-whitespace
install_vim_package plasticboy/vim-markdown
install_vim_package scrooloose/nerdcommenter
install_vim_package scrooloose/syntastic
install_vim_package shougo/neocomplete.vim "sudo apt-get install -y vim-nox"
install_vim_package shougo/vimproc.vim "cd ~/.vim/bundle/vimproc.vim && make; cd -"
install_vim_package vim-airline/vim-airline
install_vim_package vim-airline/vim-airline-themes
install_vim_package vim-scripts/cream-showinvisibles
install_vim_package evidens/vim-twig
# haskell
  install_vim_package eagletmt/ghcmod-vim "stack install ghc-mod"
  install_vim_package neovimhaskell/haskell-vim

cat > ~/.vimrc <<"EOF"
execute pathogen#infect()
filetype plugin indent on
syntax on

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
  let g:airline_theme='solarized'

" remove autoindentation when pasting
  set pastetoggle=<F2>

" neocomplete
  let g:neocomplete#enable_at_startup = 1

let g:vim_markdown_folding_disabled = 1
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:NERDSpaceDelims = 1


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

" save file shortcuts
  nmap <C-s> :update<Esc>
  inoremap <C-s> <Esc>:update<Esc>i<right>

" copy - paste between files
  vmap <leader>y :w! /tmp/vitmp<CR>
  nmap <leader>p :r! cat /tmp/vitmp<CR>

" check haskell on save and open (eagletmt/ghcmod-vim)
  autocmd BufWritePost *.hs :GhcModCheckAsync
  autocmd BufReadPost *.hs :GhcModCheckAsync

" improve the 'preview window' behaviour
  autocmd CompleteDone * pclose " close when done
  set splitbelow " move to the bottom

" flake8
  let g:flake8_show_quickfix=0  " don't show quickfix
EOF

echo "finished provisioning"