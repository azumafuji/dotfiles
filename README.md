dotfile
=======

Just some of my configs.  I'm collecting notes here on my configuration as well. 

Homebrew or something else
--------------------------
I've switched to using pkgsrc for installing alternative packages and tools not available by default in OS X.  There are some challenges to overcome primarily around paths and installing newer versions of packages.


Pyenv
-----
I install pyenv with pyenv-installer and set up my bashrc with the init calls.  I use this format of command to compile Python on my machine to ensure it's using the pkgsrc packages and also builds the framework support for OS X.

`PYTHON_CONFIGURE_OPTS="--enable-framework" CFLAGS="-O2 -I/opt/pkg/include -I/opt/pkg/include/openssl" LDFLAGS="-L/opt/pkg/lib" pyenv install -v 3.5.2`
