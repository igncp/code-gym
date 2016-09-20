#!/bin/bash

chmod -R +x /project/scripts

cp_file_from_provision() { FILE=$1; TO_PATH=$2; cp /project/provision/$FILE $TO_PATH/$FILE; }

cp_file_from_provision .bashrc ~
cp_file_from_provision .tmux.conf ~
cp_file_from_provision .vimrc ~

mkdir -p ~/logs

if ! type jq > /dev/null  ; then
  echo "installing basic packages"
  sudo apt-get update
  sudo apt-get install -y curl git unzip ack-grep git-extras \
    build-essential python-software-properties tree jq

  git config --global user.email "foo@bar.com" && git config --global user.name "Foo Bar"
fi

if [ ! -d ~/src ]; then cp -r /project/src ~; fi

if ! type stack > /dev/null  ; then
  echo "installing haskell"
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list && \
    sudo apt-get update && sudo apt-get install stack -y && \
    stack setup && \
    stack upgrade --git && \
    /project/scripts/install-repo-dependencies.sh
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
if [ ! -f ~/.vim/autoload/pathogen.vim ]; then curl https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim > ~/.vim/autoload/pathogen.vim; fi

install_vim_package plasticboy/vim-markdown
install_vim_package vim-airline/vim-airline
install_vim_package vim-airline/vim-airline-themes
install_vim_package scrooloose/syntastic
install_vim_package ctrlpvim/ctrlp.vim
install_vim_package jiangmiao/auto-pairs
install_vim_package ntpeters/vim-better-whitespace
install_vim_package scrooloose/nerdcommenter
install_vim_package vim-scripts/cream-showinvisibles
install_vim_package nathanaelkane/vim-indent-guides
install_vim_package airblade/vim-gitgutter
install_vim_package shougo/neocomplete.vim "sudo apt-get install -y vim-nox"
install_vim_package elzr/vim-json
install_vim_package shougo/vimproc.vim "cd ~/.vim/bundle/vimproc.vim && make; cd -"
install_vim_package eagletmt/ghcmod-vim "stack install ghc-mod" # haskell
install_vim_package €neovimhaskell/haskell-vim # haskell
install_vim_package milkypostman/vim-togglelist

echo "finished provisioning"
