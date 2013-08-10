export EDITOR=mate
export TERM=xterm-256color
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1 
export LESSOPEN="| ~/.source-highlight/src-hilite-lesspipe.sh %s"
export LESS=" -R "
export SOURCE_HIGHLIGHT_DATADIR=~/.source-highlight/
export JAVA_HOME=$(/usr/libexec/java_home)
export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
export EC2_HOME="/usr/local/Library/LinkedKegs/ec2-api-tools/jars"
export PROXY_PYPI_DIR="$HOME/Dev/pypicache"


ulimit -n 4096 

export DJANGO_COLORS="light"

function _update_ps1()
{
   export PS1="$(~/.powerline-shell.py $?)"
}

export PROMPT_COMMAND="_update_ps1"

source /usr/local/bin/virtualenvwrapper.sh

alias rcp='rsync -aP'
alias cleanpyc='find . -name '*.pyc' -exec rm {} \;'

alias startmongo='mongod run --rest --config ~/.mongodb/mongod.conf'
alias startes='elasticsearch -f -D es.config=/Users/dean/.elasticsearch/elasticsearch.yml'
alias connectencota='ssh -i ~/.ssh/PraxisProduction.pem ubuntu@encota.praxismi.com'
alias em='editmoin --trivial-change'
alias usecontext='. /Users/dean/Dev/ConTeXt/tex/setuptex'
alias serveit='python -m SimpleHTTPServer 8080'
alias timestamp='date "+%Y%m%dT%H%M%S"'
alias tnote='/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier -title "iTerm2" -message '
alias brewupgrade='brew upgrade; brew cleanup; tnote "homebrew upgraded."'
alias ppp='proxypypi -p 8333 -d $PROXY_PYPI_DIR -P $PROXY_PYPI_DIR/proxypypi.pid -l $PROXY_PYPI_DIR/proxypypi.log -o $PROXY_PYPI_DIR/proxypypi.console'

export PATH=~/.cabal/bin:/usr/local/share/npm/bin:$PATH

if [ -f .local.env ]; then
  source .local.env
fi



PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
