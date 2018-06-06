#!/bin/bash
# -*- mode: sh-mode -*-
alias emacs='/usr/local/Cellar/emacs/26.1_1/bin/emacsclient -nw'
export EDITOR='emacs'
export TERM=xterm-256color
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1 
export LESS=" -R "
export JAVA_HOME=$(/usr/libexec/java_home)

export LESSOPEN="| ~/src/dotfiles/src-hilite-lesspipe.sh %s"

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}


# Need to configure these 
# export AWS_ACCESS_KEY="<Your AWS Access ID>"
# export AWS_SECRET_KEY="<Your AWS Secret Key>"
# export AWS_CREDENTIAL_FILE="<Path to the credentials file>"

export VAGRANT_DEFAULT_PROVIDER="vmware_fusion"
export MP_FULLNAME="Dean J Sellis"
export CLOUDSDK_PYTHON='/usr/bin/python'

ulimit -n 4096 

alias rcp='rsync -aP'
alias cleanpyc='find . -name '*.pyc' -exec rm {} \;'

alias serveit='python -m SimpleHTTPServer 8080'
alias timestamp='date "+%Y%m%dT%H%M%S"'
alias updaterepos='find . -maxdepth 1 -type d -print -execdir git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;'

alias joinPDF='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'



# AWS SSH Aliases

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8


if [ -f ~/src/liquidprompt/liquidprompt ]; then
    source ~/src/liquidprompt/liquidprompt
fi


if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi


if [ -f .local.env ]; then
  source .local.env
fi

# Configure NVM
export NVM_DIR="/Users/dean/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm


# Configure Pyenv
export PATH="/Users/dean/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Configure Rust
source $HOME/.cargo/env

# Configure Go
export PATH="$PATH:/Users/dean/go/bin:/usr/local/opt/go/libexec/bin"
export GOPATH="/Users/dean/go"

# HH config for fancy history
export HH_CONFIG=hicolor,keywords         # get more colors
shopt -s histappend              # append new history items to .bash_history
export HISTCONTROL=ignorespace:ignoredups:erasedups   # leading space hides commands from history
export HISTFILESIZE=9216         # increase history file size (default is 500)
export HISTSIZE=${HISTFILESIZE}  # increase history size (default is 500)
export HISTTIMEFORMAT="%h %d %H:%M:%S> "
export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"   # mem/file sync
# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* ]]; then bind '"\C-r": "\C-a hh -- \C-j"'; fi






