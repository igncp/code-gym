#!/bin/bash

GLOBAL_NPM_MODULES=(http-server rethinkdb bluebird ramda)

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
linkModulesInPath() {
  DIR_PATH=$1
  for MODULE_NAME in "${GLOBAL_NPM_MODULES[@]}"; do
    if [ ! -d $DIR_PATH/node_modules/$MODULE_NAME ]; then cd $DIR_PATH && npm link $MODULE_NAME  > /dev/null && cd - > /dev/null; fi
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

for MODULE_NAME in "${GLOBAL_NPM_MODULES[@]}"; do
  if [ ! -d ~/.nodenv/versions/6.3.0/lib/node_modules/$MODULE_NAME ]; then
    echo "doing: npm i -g $MODULE_NAME"
    npm i -g $MODULE_NAME
  fi
done

if [ ! -d ~/logs ]; then
  mkdir ~/logs
fi

if ! type rethinkdb > /dev/null  ; then
  echo "installing rethinkdb"
  source /etc/lsb-release && echo "deb http://download.rethinkdb.com/apt $DISTRIB_CODENAME main" | sudo tee /etc/apt/sources.list.d/rethinkdb.list
  wget -qO- https://download.rethinkdb.com/apt/pubkey.gpg | sudo apt-key add -
  sudo apt-get update
  sudo apt-get install -y rethinkdb
  sudo rm -rf /var/lib/rethinkdb/instances.d/default
  sudo rethinkdb create -d /var/lib/rethinkdb/instances.d/default 2>&1
fi

sudo cp /project/provision/default.conf /etc/rethinkdb/instances.d/
linkModulesInPath /project/examples/basic

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
