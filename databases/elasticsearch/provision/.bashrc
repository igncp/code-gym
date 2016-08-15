if [[ -z $TMUX ]]; then tmux; fi

bind '"\C-f":vi-fWord'
bind '"\C-b":vi-bWord'

if [[ -z $TMUX ]]; then TMUX_PREFIX="·"; else TMUX_PREFIX="£"; fi
PS1='${debian_chroot:+($debian_chroot)}\n\u@\h: \W$(__git_ps1) $TMUX_PREFIX '

alias rm="rm -rf"
alias ll="ls -lah"

export JAVA_HOME=/usr/lib/jvm/java-8-oracle/

export PATH=$PATH:/project/scripts
export PATH=$PATH:/home/vagrant/elasticsearch/bin
export PATH=$PATH:/home/$USER/.nodenv/bin
export PATH=$PATH:/home/$USER/.nodenv/versions/6.3.0/bin/

eval "$(nodenv init -)"
source <(npm completion)

