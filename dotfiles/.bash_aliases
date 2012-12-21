if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ll='ls -l'
alias la='ls -la'
alias mv='mv -v'
alias rm='rm -v'
alias cp='cp -v'
alias client='emacsclient -n'
alias sl='sl -ale'
alias LS='LS -ale'
alias make='make -j$(cat /proc/cpuinfo |grep 'processor' |wc -l)'
alias mmake='colormake -j$(cat /proc/cpuinfo |grep 'processor' |wc -l)'
alias igrep='grep -i'

function dgrep {
    dpkg -l |grep $1
}

complete -o filenames -F _apt_cache purge
function purge {
    sudo apt-get remove --purge $@
}

function search {
    apt-cache search $@
}

complete -o filenames -F _apt_cache show
function show {
    apt-cache show $@
}

function qrurl() {
    curl "http://chart.apis.google.com/chart?chs=150x150&cht=qr&chld=H%7C0&chl=$1" -o qr.$(date +%Y%m%d%H%M%S).png;
}

function server() {
    python3 -m http.server
}

function sop () {
    (sp-sc $1 8908 8900 &>/dev/null &); sleep 10; wait $(cvlc http://localhost:8900); killall sp-sc
}

function ttail() {
    tail -f $1 | awk '{now=strftime("\033[0;33m%F %T%z\033[1;34m\t");sub(/^/, now);print}'
}

function cam() {
    mplayer -cache 128 -tv driver=v4l2:width=176:height=177 -vo xv tv:// -noborder -geometry "95%:93%" -ontop
}

function record() {
    ffmpeg -f x11grab -s wxga -r 25 -i :0.0 -sameq $1
}

function clean() {
    find . -type f \( -name '*.o' -o -name '*~' -o -name '#*#' -o -name '.#*' -o -name '*.class' \
	-o -name '*.pyc' -o -name '*.toc' -o -name '*.asv' \) -delete
}

function psgrep() {
    ps aux |grep -i $1
}

function isomount() {
    mount -o loop -t iso9660 $1 $2
}

alias rscp='rsync --partial --progress --rsh=ssh'
alias dmesg='dmesg |grep -v UFW'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# git
# git_zip <branch> <output>
function git_zip() {
    git archive --format zip --output $2 $1
}

function git_gz() {
    git archive --format tar $1 | gzip > $2
}

function git_bz() {
    git archive --format tar $1 | bzip2 > $2
}
