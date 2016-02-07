# ~/.bashrc: executed by bash(1) for non-login shells.

#
# ENVIRONMENT
#

export DEBFULLNAME="Alex Griffin"
export DEBEMAIL="a@ajgrf.com"

export EDITOR="vim"

export GOROOT="$HOME/.local/go"
export GOPATH="$HOME/.local:$HOME"

# Locale Settings
test "$LANG" = "zh_TW.utf8" && export LANGUAGE="zh_TW.utf8:zh_CN.utf8"

#
# PATH
#

PREPATH="$HOME/.local/bin:$GOROOT/bin"
for pkg in alex/3.1.4 cabal/1.22 ghc/7.10.2 happy/1.19.5; do
	POSTPATH="$POSTPATH:/opt/${pkg}/bin"
done
POSTPATH="${POSTPATH#:}"
PATH="$PREPATH:${PATH#$PREPATH:}"
PATH="${PATH%:$POSTPATH}:$POSTPATH"
unset PREPATH POSTPATH

#
# INTERACTIVE SHELL SETTINGS
#

# If not running interactively, don't do anything
case $- in
*i*)
	;;
*)
	return
esac

# history settings
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
HISTCONTROL=ignoreboth
HISTIGNORE='l:l[salhc]:cd:clear:reset'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

#
# PROMPT
#

PROMPT_DIRTRIM=3

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
	debian_chroot=$(cat /etc/debian_chroot)
fi

precmd() {
	local e=$?

	if [ $e -ne 0 ]; then
		echo -n "$e|"
	fi
}

PS1='${debian_chroot:+($debian_chroot)}$(precmd)\u@\h:\w\$ '

#
# TERMINAL TITLE
#

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
esac

#
# ALIASES, FUNCTIONS, ETC.
#

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
# disable less history file
export LESSHISTFILE=/dev/null
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "

alias doch='eval sudo $(fc -ln -1)'

# some ls aliases
alias ls='LC_COLLATE=C ls -A1 --time-style=long-iso'
alias lc='LC_COLLATE=C command ls -Ap --time-style=long-iso'
alias l='lc'
alias la='ls'
alias ll='ls -la'
alias lh='ll -h'
alias open='xdg-open'
alias nb='newsbeuter'
alias gpg='gpg2'
alias ag='ag --nocolor --nogroup'
alias emacs='emacs -nw'
alias apt='sudo apt'

# open vim help
:h() {
	vim -c ":help $*| only"
}

# create a directory (if necessary) and cd into it
mkcd() {
	mkdir -p "$@" && cd "$1"
}

godoc() {
	command godoc "$@" |less
}
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

article-convert() {
	local title
	for article in *.html; do
		title="${article%.html}"
		ebook-convert "$article" "${title}.mobi" &&
		trash         "$article" "${title}-Dateien"
	done
}
