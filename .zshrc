echo "FIRST +++ $PATH"

# This .zshrc file is meant to be used with oh-my-zsh
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="perso"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Zsh options go there

# Since we're using a fork, we do not rely on auto-updates
DISABLE_AUTO_UPDATE=true

# No SIGHUP on shell exit
NO_HUP=true

# History stuff
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

APPEND_HISTORY=true
INC_APPEND_HISTORY=true
SHARE_HISTORY=true
EXTENDED_HISTORY=true
HIST_IGNORE_DUPS=true
HIST_REDUCE_BLANKS=true
HIST_NO_STORE=true
NO_HIST_BEEP=true

# Enable zsh auto-correct
setopt correct

# Various options
setopt nobeep
setopt cdablevars
setopt multios

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git cp encode64 extract gem git history mercurial perl python svn)

source $ZSH/oh-my-zsh.sh

# Emacs bindkeys please
bindkey -e

# Used later by script in order to launch the right shell
export SHELL="$(which zsh)"

# Logs all shell sessions using script
ps -p $PPID | grep script &>/dev/null
ps_retval=$?
if [[ "$ps_retval" -ne 0 ]]; then
    ZSH_SESSION_LOG_DIR=~/.logs/typescripts
    mkdir -p $ZSH_SESSION_LOG_DIR
    local filename=$ZSH_SESSION_LOG_DIR/typescript_$(date +"%d_%m_%y-%k_%M-%S" | tr -d ' ')
    echo "A typescript of this session will be saved as $filename."
    script -f --force -q "$filename"
else
    # Initial PATH, not set again inside the script session
    export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:$HOME/bin:$PATH
fi

export TERM=xterm-256color

if [ -n "$INSIDE_EMACS" ]; then
    PROMPT="%#> "
fi

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi

if [ -d ~/.zshrc.d ]; then
    for file in ~/.zshrc.d/*.zsh; do
        source ${file}
    done
    if [ -d ~/.zshrc.d/functions ]; then
        for file in ~/.zshrc.d/functions/*.zsh; do
            source ${file}
        done
    fi
fi
