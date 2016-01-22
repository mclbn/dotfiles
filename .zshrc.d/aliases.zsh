# overriding previous aliases
alias ls='ls -F'
alias grep='grep'
alias ln='ln'

alias setenv="export"
alias ll='ls -l'
alias l='ll'
alias c='cd'
alias dir='ls'
alias objdump='objdump -M intel -D'
alias gdb='gdb -q'
alias mp='mkdir -p'
alias ssh='ssh -o ServerAliveInterval=60 -o ServerAliveCountMax=10 -o UseRoaming=no'
alias grep_urls="grep -aiRPoH 'https?://[a-zA-Z0-9\-\_\.\~\!\*'\''\(\)\;\:\@\&\=\+\$\,\/\?\#\[\]\%]+'|sed 's/:/,/'"
alias grep_full='grep -aiRPoH'
alias wget_no_cert_verify 'wget --no-check-certificate'5D
alias tree='tree | less'
alias chmox='chmod +x'
alias emacs='emacs -nw'
alias ne='emacs -Q'

alias git_update_fork='git pull && git fetch upstream && git merge upstream/master && git push origin master'
