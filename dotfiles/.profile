# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

####################################### PATH
if [ -d "/usr/bin" ] ; then
    PATH="/usr/bin:$PATH"
fi

if [ -d "/usr/local/bin" ] ; then
    PATH="/usr/local/bin:$PATH"
fi

if [ -d "/sbin" ] ; then
    PATH="/sbin:$PATH"
fi

if [ -d "/usr/sbin" ] ; then
    PATH="/usr/sbin:$PATH"
fi

if [ -d "$HOME/usr/bin" ] ; then
    PATH="$HOME/usr/bin:$PATH"
fi

if [ -d "/usr/numeca/bin" ] ; then
    PATH="/usr/numeca/bin:$PATH"
    # export NI_DRIVER=X11
fi

if [ -d "$HOME/work/android-sdk-linux_x86/tools" ] ; then
    PATH="$HOME/work/android-sdk-linux_x86/tools:$PATH"
fi

if [ -d "$HOME/work/android-sdk-linux_x86/platform-tools" ] ; then
    PATH="$HOME/work/android-sdk-linux_x86/platform-tools:$PATH"
fi

if [ -d "/opt/intel/bin" ] ; then
    PATH="/opt/intel/bin:$PATH"
fi

if [ -d "$HOME/scripts" ] ; then
    PATH="$HOME/scripts:$PATH"
fi

if [ -d "$HOME/games/bin" ] ; then
    PATH="$HOME/games/bin:$PATH"
fi

export PATH
#########################################

## Turn off bell
xset b off

if [ -f "/usr/share/terminfo/r/rxvt-unicode-256color" ]; then
    export TERM=rxvt-unicode-256color
else
    export TERM=xterm
fi

# CDPATH
export CDPATH='~:~/school:~/work'

# LANGUAGE
# export LANGUAGE="fr:en_GB:en"
# export LC_MESSAGES="fr_FR.UTF-8"
# export LC_CTYPE="fr_FR.UTF-8"
# export LC_COLLATE="fr_FR.UTF-8"
# export LANG="fr_FR.UTF-8"

xrdb -load ~/.Xresources
