#!/bin/bash

cpFileFromProvision() { FILE=$1; TO_PATH=$2; cp /project/provision/$FILE $TO_PATH/$FILE; }
installVimPackage() {
  REPO=$1
  DIR=$(echo $REPO | sed -r "s|.+/(.+)|\1|") # foo/bar => bar
  EXTRA_CMD=$2
  if [ ! -d ~/.vim/bundle/"$DIR" ]; then
    git clone https://github.com/$REPO.git ~/.vim/bundle/"$DIR"
    if [[ ! -z $EXTRA_CMD ]]; then eval $EXTRA_CMD; fi
  fi
}

cpFileFromProvision .bashrc ~
cpFileFromProvision .tmux.conf ~
cpFileFromProvision .vimrc ~

if ! type tree > /dev/null  ; then
  echo "installing basic packages"
  sudo apt-get update
  sudo apt-get install -y vim-nox # for vim neocomplete
  sudo apt-get install -y curl git unzip ack-grep git-extras jq tree

  git config --global user.email "you@example.com" && git config --global user.name "Your Name"
fi

if [ ! -d ~/logs ]; then
  mkdir ~/logs
fi

mkdir -p ~/.vim/autoload/ ~/.vim/bundle
if [ ! -f ~/.vim/autoload/pathogen.vim ]; then curl https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim > ~/.vim/autoload/pathogen.vim; fi

installVimPackage plasticboy/vim-markdown
installVimPackage scrooloose/syntastic
installVimPackage ctrlpvim/ctrlp.vim
installVimPackage jiangmiao/auto-pairs
installVimPackage ntpeters/vim-better-whitespace
installVimPackage scrooloose/nerdcommenter
installVimPackage vim-scripts/cream-showinvisibles
installVimPackage nathanaelkane/vim-indent-guides
installVimPackage airblade/vim-gitgutter
installVimPackage shougo/neocomplete.vim
installVimPackage shougo/vimproc.vim "cd ~/.vim/bundle/vimproc.vim && make; cd -"

echo "finished provisioning"
