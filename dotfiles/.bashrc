# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
# export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# History size
HISTSIZE=1337
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# Git PS1
if [ -r /etc/bash_completion.d/git ]; then

    . /etc/bash_completion.d/git

    GIT_PS1_SHOWDIRTYSTATE=1     # untagged(*) and staged(+) changes
    GIT_PS1_SHOWSTASHSTATE=1     # if something is stashed($)
    GIT_PS1_SHOWUNTRACKEDFILES=1 # untracked files(%)

    if [ "$color_prompt" = yes ]; then
        PS1='\[\e[0;33m\][\j]\[\e[1;36m\] \u\[\e[1;33m\]@\[\e[1;35m\]\h\[\e[0;36m\] [\w]\[\e[0;33m\]$(__git_ps1)\[\e[1;33m\] \$ \[\e[m\]'
    else
        PS1='\[\e[0;33m\][\j]\[\e[1;36m\] \u\[\e[1;33m\]@\[\e[1;35m\]\h\[\e[0;36m\] [\w]\[\e[0;33m\]$(__git_ps1)\[\e[1;33m\] \$ \[\e[m\]'
    fi

else

    if [ "$color_prompt" = yes ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
# case "$TERM" in
#     xterm*|rxvt*)
#       PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#       ;;
#     *)
#       ;;
# esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    elif [ -f /use/share/bash-completion/bash_completion ]; then
        . /use/share/bash-completion/bash_completion
    fi
fi

# Implicit directory change
shopt -s autocd

PATH=~/usr/bin:$PATH
