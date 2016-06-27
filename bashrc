#!/bin/bash
# -*- mode: sh-mode -*-
export EDITOR='emacsclient -nw'
export TERM=xterm-256color
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1 
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=" -R "
export JAVA_HOME=$(/usr/libexec/java_home)

# Need to configure these 
export AWS_ACCESS_KEY="<Your AWS Access ID>"
export AWS_SECRET_KEY="<Your AWS Secret Key>"
export AWS_CREDENTIAL_FILE="<Path to the credentials file>"

export VAGRANT_DEFAULT_PROVIDER="vmware_fusion"

ulimit -n 4096 

alias rcp='rsync -aP'
alias cleanpyc='find . -name '*.pyc' -exec rm {} \;'

alias serveit='python -m SimpleHTTPServer 8080'
alias timestamp='date "+%Y%m%dT%H%M%S"'
alias updaterepos='find . -maxdepth 1 -type d -print -execdir git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;'
alias e='emacsclient -c -n'


if [ -f ./src/liquidprompt/liquidprompt ]; then
    source src/liquidprompt/liquidprompt
fi

if [ -f .local.env ]; then
  source .local.env
fi

# Configure bash_completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Configure NVM
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

# Configure Pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
