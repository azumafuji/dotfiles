#!/bin/bash

java -version

sudo /usr/libexec/locate.updatedb

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install caskroom/cask/brew-cask

brew cask install dropbox
brew cask install flycut
brew cask install google-chrome
brew cask install google-drive
brew cask install screenhero
brew cask install vagrant
brew cask install spectacle
brew cask install flux
brew cask install pandoc
brew cask install whatsapp
brew cask install sonos
brew cask install sourcetree
brew cask install visual-studio-code

# brew install emacs  --with-cocoa --with-gnutls --with-imagemagick --with-librsvg --with-mailutils --devel

brew install git
# This needs java!
brew install graphviz --with-app --with-bindings --with-freetype --with-gts --with-librsvg --with-pango
brew install source-highlight
brew install the_silver_searcher
brew install xscreensaver
brew install sox --with-flac --with-lame --with-libao --with-libsndfile --with-libvorbis --with-opencore-amr --with-opusfile

sudo cp ~/src/dotfiles/paths /etc/paths

mkdir ~/.ec2

mkdir ~/.git

ln -s ~/src/dotfiles/gitconfig ~/.gitconfig
ln -s ~/src/dotfiles/gitignore_global ~/.gitignore_global

mkdir ~/.lein
ln -s ~/src/dotfiles/profiles.clj ~/.lein/profiles.clj


mkdir ~/Documents/iPython

mkdir ~/Documents/org
mkdir -p ~/.emacs.d/snippets
ln -s ~/src/dotfiles/lisp ~/.emacs.d/lisp
ln -s ~/src/dotfiles/emacs ~/.emacs


ipython profile create
rm ~/.ipython/profile_default/ipython_config.py
rm ~/.ipython/profile_default/ipython_notebook_config.py
ln -s ~/src/dotfiles/ipython_config.py ~/.ipython/profile_default/ipython_config.py
ln -s ~/src/dotfiles/ipython_notebook_config.py ~/.ipython/profile_default/ipython_notebook_config.py

mkdir ~/.pip
ln -s ~/src/dotfiles/pip.conf ~/.pip/pip.conf
ln -s ~/src/dotfiles/pydistutils.cfg ~/.pydistutils.cfg

ln -s ~/src/dotfiles/bashrc ~/.bashrc
ln -s ~/src/dotfiles/profile ~/.profile
ln -s ~/src/dotfiles/bash_profile ~/.bash_profile

git clone git://github.com/chriskempson/tomorrow-theme.git ~/src/tomorrow-theme

#sh ~/src/dotfiles/osx.sh

