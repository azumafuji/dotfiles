#!/bin/bash
# -*- mode: sh-mode -*-
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'

export EDITOR='emacs'
export TERM=xterm-256color
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1 
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

# Aliases for the installed emacs package
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw "$@"'
alias e='/Applications/Emacs.app/Contents/MacOS/Emacs "$@" &'


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

