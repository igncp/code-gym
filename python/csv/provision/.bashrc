# move from word to word. avoid ctrl+b to use in tmux
  bind '"\C-g":vi-fWord' > /dev/null 2>&1
  bind '"\C-f":vi-bWord' > /dev/null 2>&1

stty -ixon # prevent the terminal from hanging on ctrl+s

if [[ -z $TMUX ]]; then TMUX_PREFIX="·"; else TMUX_PREFIX="£"; fi
PS1='${debian_chroot:+($debian_chroot)}\n\u@\h: \W$(__git_ps1) $TMUX_PREFIX '

export PATH=$PATH:/project/scripts
export PATH=$PATH:/project/provision
export PATH=$PATH:~/.local/bin

if [ -d ~/src ]; then cd ~/src; fi

alias ll="ls -lah"
alias rm="rm -rf"
alias mkdir="mkdir -p"
alias cp="cp -r"
alias tmux="tmux; exit"
