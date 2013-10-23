

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

brew tap marcqualie/nginx
brew tap phinze/homebrew-cask

brew install brew-cask

brew cask install caffeine
brew cask install cord
brew cask install firefox
brew cask install google-chrome
brew cask install itunes-volume-control
brew cask install jumpcut
brew cask install light-table
brew cask install pycharm
brew cask install screenhero
brew cask install size-up
brew cask install sourcetree
brew cask install super-duper
brew cask install vagrant


brew install emacs --cocoa --with-gnutls
brew install git
brew install graphviz --with-freetype --with-bindings --with-app
brew install hub
brew install leiningen
brew install memcached
brew install libmemcached
brew install nginx-full --with-spdy --with-lua-module --with-webdav
brew install postgresql 
brew install pypy
brew install rabbitmq
brew install redis
brew install rethinkdb
brew install source-highlight
brew install the_silver_searcher
brew install zeromq

ln -s ~/src/dotfiles/bashrc ~/.bashrc
ln -s ~/src/dotfiles/profile ~/.profile
ln -s ~/src/dotfiles/bash_profile ~/.bash_profile


mkdir ~/.git

ln -s ~/src/dotfiles/gitconfig ~/.gitconfig
ln -s ~/src/dotfiles/gitignore_global ~/.gitignore_global

git clone git@github.com:milkbikis/powerline-shell.git ~/src/powerline-shell
ln -s src/powerline-shell/powerline-shell.py ~/.powerline-shell.py

mkdir ~/.lein
ln -s ~/src/dotfiles/profiles.clj ~/.lein/profiles.clj

mkdir ~/.pip
ln -s ~/src/dotfiles/pip.conf ~/.pip/pip.conf

ln -s ~/src/dotfiles/pydistutils.cfg ~/.pydistutils.cfg

ln -s ~/src/dotfiles/source-highlight ~/.source-highlight

mkdir ~/src/ipython
mkdir ~/.notes
mkdir -p ~/.emacs.d/snippets

ln -s ~/src/dotfiles/emacs ~/.emacs

sudo easy_install pip 
sudo pip install virtualenv virtualenvwrapper
sudo pip install rope ropemacs
sudo pip install pyzmq
sudo pip install readline ipython
sudo pip install proxypypi
sudo pip install jinja2 tornado

git clone git@github.com:pinard/Pymacs.git ~/src/Pymacs
cd ~/src/Pymacs
make
sudo python setup.py install
