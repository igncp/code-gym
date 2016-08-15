#!/bin/bash

log() { echo "$1\n"; echo "\n$1\n" >> $LOG_FILE; }
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
installNPMModulesGlobally() {
  for MODULE_NAME in "$@"; do
    if [ ! -d ~/.nodenv/versions/6.3.0/lib/node_modules/$MODULE_NAME ]; then
      echo "doing: npm i -g $MODULE_NAME"
      npm i -g $MODULE_NAME
    fi
  done
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

if ! type java > /dev/null ; then
  echo "installing java 8"
  sudo add-apt-repository -y ppa:webupd8team/java
  sudo apt-get update
  echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
  sudo apt-get install -y oracle-java8-installer
fi

if ! type elasticsearch > /dev/null ; then
  echo "installing elasticsearch"
  ELASTIC_NAME=elasticsearch-2.3.4
  ELASTIC_FILE="$ELASTIC_NAME".tar.gz
  cd ~
  if [ ! -d ./elasticsearch ]; then
    if [ ! -f ~/$ELASTIC_FILE ]; then curl -L -O https://download.elastic.co/elasticsearch/release/org/elasticsearch/distribution/tar/elasticsearch/2.3.4/$ELASTIC_FILE; fi
    tar -xvf $ELASTIC_FILE
    rm $ELASTIC_FILE
    mv $ELASTIC_NAME elasticsearch
  fi
fi

cpFileFromProvision elasticsearch.yml ~/elasticsearch/config

if [ ! -f ~/node-installation-finished ]; then
  echo "setup node with nodenv"
  sudo add-apt-repository -y ppa:chris-lea/node.js && \
    sudo apt-get update && \
    sudo curl -O -L https://npmjs.org/install.sh | sh && \
    if [ ! -d ~/.nodenv ]; then git clone https://github.com/nodenv/nodenv.git ~/.nodenv && cd ~/.nodenv && src/configure && make -C src; fi && \
    export PATH=$PATH:/home/$USER/.nodenv/bin && \
    eval "$(nodenv init -)" && \
    if [ ! -d ~/.nodenv/plugins/node-build ]; then git clone https://github.com/nodenv/node-build.git $(nodenv root)/plugins/node-build; fi && \
    if [ ! -d .nodenv/versions/6.3.0 ]; then nodenv install 6.3.0; fi && \
    nodenv global 6.3.0 && \
    touch ~/node-installation-finished
fi

installNPMModulesGlobally http-server

if [ ! -d ~/elasticsearch-HQ ]; then
  echo "installing elasticsearch-HQ (gui)"
  git clone https://github.com/royrusso/elasticsearch-HQ.git ~/elasticsearch-HQ
  cd ~/elasticsearch-HQ
  npm i
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
