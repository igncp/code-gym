bind '"\C-f":vi-fWord'
bind '"\C-b":vi-bWord'

if [[ -z $TMUX ]]; then TMUX_PREFIX="·"; else TMUX_PREFIX="£"; fi
PS1='${debian_chroot:+($debian_chroot)}\n\u@\h: \W$(__git_ps1) $TMUX_PREFIX '

alias rm="rm -rf"
alias ll="ls -lah"

export PATH=$PATH:/project/scripts

