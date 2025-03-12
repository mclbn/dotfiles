# overriding previous aliases
alias ls='ls -F --color'
alias grep='grep'
alias ln='ln'

alias setenv="export"
alias ll='ls -lrt'
alias l1='ls -l1r'
alias l='ll'
alias lh='ls -lh'
alias c='cd'
alias fd='find'
alias feh='feh -.'
alias 'hr'='hash -r'
alias diff='diff --color=auto'
alias dir='ls'
alias objdump='objdump -M intel -D'
alias gdb='gdb -q'
alias history='history 1'
alias ip='ip -color=auto'
alias mp='mkdir -p'
alias ssh='ssh -o ServerAliveInterval=60 -o ServerAliveCountMax=10 -o UseRoaming=no'
alias sshx='ssh -X -Y -C -c blowfish-cbc,arcfour'
alias grep='grep --color=auto'
alias grep_urls="grep -aiRPoH 'https?://[a-zA-Z0-9\-\_\.\~\!\*'\''\(\)\;\:\@\&\=\+\$\,\/\?\#\[\]\%]+'|sed 's/:/,/'"
alias grep_full='grep -aiRPoH'
alias wget_no_cert_verify 'wget --no-check-certificate'
#alias tree='tree | less'
alias chmox='chmod +x'
alias emacs='emacs -nw'
alias ne='emacs -Q'
alias rag='ranger'
alias rg='rg --color=auto'
alias rga='rga --color=auto'
alias py='python'
alias py2='python2'
alias py3='python3'
alias tx='tmux a || tmux'
alias z='zathura'
alias zzz='systemctl suspend && /home/marc/bin/lock'
alias ZZZ='systemctl hibernate'

alias git_update_fork='git pull && git fetch upstream && git merge upstream/master && git push origin master'
alias ipython='ipython3'

if [[ $(uname) == "OpenBSD" ]]; then
	alias shred='rm -P'
	alias shred_all='find . -type f -exec rm -P {} \;'
else
	alias shred_all='find . -type f -exec shred -u {} \;'
fi

alias docker_cleanup='sudo docker-gc ; docker volume ls -qf dangling=true | xargs -r docker volume rm'
alias docker_volume_cleanup='docker volume ls -qf dangling=true | xargs -r docker volume rm'

alias cmake-release='cmake -DCMAKE_BUILD_TYPE=Release'
alias cmake-debug='cmake -DCMAKE_BUILD_TYPE=Debug'
