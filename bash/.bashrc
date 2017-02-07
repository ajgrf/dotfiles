# ~/.bashrc: executed by bash(1) for non-login shells.

#
# ENVIRONMENT
#

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
#export SHELL

# Set $PATH and friends in non-interactive SSH sessions.
if [ -n "$SSH_CLIENT" -a -z "`type -P cat`" ]; then . /etc/profile; fi

#
# INTERACTIVE SHELL SETTINGS
#

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
shopt -s globstar

#
# PROMPT
#

PROMPT_DIRTRIM=3

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
	debian_chroot=$(cat /etc/debian_chroot)
fi

if_err() {
	local e=$?
	if [ $e -ne 0 ]; then
		echo -n "$e|"
	fi
}

PS1='${debian_chroot:+($debian_chroot)}$(if_err)\u@\h:\w${GUIX_ENVIRONMENT:+ [env]}\$ '

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
hash lesspipe 2> /dev/null && eval "$(SHELL=/bin/sh lesspipe)"
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
#alias gpg='gpg2'
alias ag='ag --nocolor --nogroup'
alias emacs='emacs -nw'

# open vim help
:h() {
	vim -c ":help $*| only"
}

# create a directory (if necessary) and cd into it
mkcd() {
	mkdir -p "$@" && cd "$1"
}

article-convert() {
	local title
	pushd "${1:-$HOME/Downloads}" > /dev/null
	mkdir -p "$HOME/Downloads/articles"
	for article in *.html; do
		title="${article%.html}"
		ebook-convert "$article" "$HOME/Downloads/articles/${title}.mobi" &&
		trash "$article" "${title}_files"
	done
	popd > /dev/null
}

guix() {
	local fmtstr guixdir guixenv
	guixdir="${XDG_CONFIG_HOME:-$HOME/.config}/guix"

	guixenv="$guixdir/latest/pre-inst-env"
	if [ ! -x "$guixenv" ]; then
		guixenv="env"
	fi

	case "$1" in
	add|install)
		shift
		"$guixenv" guix package --install "$@"
		;;
	env)
		shift
		"$guixenv" guix environment "$@"
		;;
	manifest)
		"$guixenv" guix package --manifest="${2:-$guixdir/profile.scm}"
		;;
	profile)
		shift
		"$guixenv" guix package "$@"
		;;
	reconfigure)
		"$guixenv" sudo guix system reconfigure "${2:-$guixdir/system.scm}"
		;;
	remove|uninstall)
		shift
		"$guixenv" guix package --remove "$@"
		;;
	repl)
		shift
		"$guixenv" guile "$@"
		;;
	search)
		fmtstr="{{name}} {{version}} - {{synopsis}}"
		"$guixenv" guix package --search="$2" |
			recfmt -f <(echo "$fmtstr") |
			uniq
		;;
	show|info)
		"$guixenv" guix package --show="$2"
		;;
	try)
		shift
		"$guixenv" guix environment --ad-hoc "$@"
		;;
	*)
		"$guixenv" guix "$@"
		;;
	esac
}

mkguix() (
	cd ~/src/guix
	guix environment guix -- make
)

remkguix() {
	rm ~/.config/guix/latest
	guix pull
	guix environment guix -- bash -c 'cd ~/src/guix && git clean -fdx && ./bootstrap && ./configure --localstatedir=/var && make && git checkout po/'
	rm ~/.config/guix/latest
	ln -s ../../src/guix ~/.config/guix/latest
}
