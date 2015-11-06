alias setenv="export"
# alias ls='ls --color'
alias ll='ls -l'
alias l='ll'
alias c='cd'
alias dir='ls'
alias objdump='objdump -M intel -D'
alias gdb='gdb -q'
alias mp='mkdir -p'
alias ssh='ssh -o ServerAliveInterval=60 -o ServerAliveCountMax=10'
#alias grep='grep --color=always'
alias grep_urls="grep -aiRPoH --color=never 'https?://[a-zA-Z0-9\-\_\.\~\!\*'\''\(\)\;\:\@\&\=\+\$\,\/\?\#\[\]\%]+'|sed 's/:/,/'"
alias grep_full='grep -aiRPoH'
alias wget_no_cert_verify 'wget --no-check-certificate'5D
alias tree='tree | less'

alias wakehome="ssh root@lioks-brazil.no-ip.org -p 6589 '/usr/sbin/wol -i 192.168.1.255 -p 7 20:cf:30:c0:98:5f'"

alias git_update_fork='git pull && git fetch upstream && git merge upstream/master && git push origin master'
