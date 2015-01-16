# ~/.bashrc: executed by bash(1) for non-login shells.

#
# ENVIRONMENT
#

export DEBFULLNAME="Alex Griffin"
export DEBEMAIL="alex@alexjgriffin.com"

export LD_LIBRARY_PATH="$HOME/.local/lib"

export GOROOT="$HOME/.local/go"
export GOPATH="$HOME"

# Locale Settings
test "$LANG" = "zh_TW.utf8" && export LANGUAGE="zh_TW.utf8:zh_CN.utf8"

#
# PATH HELPER FUNCTIONS
#

# remove the given dir from $PATH
pathremove() {
	PATH=$(echo "$PATH" |
		awk -F: '{
			for (i = 1; i <= NF; i++) 
				if ($i == dir)
					$i = ""
			gsub(/:+/, ":")
			gsub(/^:|:$/, "")
			print
		}' OFS=":" dir="$1"
	)
}

# add or move the given dir to the beginning of $PATH
pathprepend() {
	if [ -d "$1" ]; then
		pathremove "$1"
		PATH="${1:+$1:}$PATH"
	else
		return 1
	fi
}

# add or move the given dir to the end of $PATH
pathappend() {
	if [ -d "$1" ]; then
		pathremove "$1"
		PATH="$PATH${1:+:$1}"
	else
		return 1
	fi
}

#
# PATH
#

pathprepend "$HOME/bin"
pathprepend "$HOME/.local/bin"
pathprepend "$GOROOT/bin"
for pkg in ghc-7.8.4 cabal-1.18 alex-3.1.3 happy-1.19.4; do
	pathprepend "/opt/${pkg%-*}/${pkg##*-}/bin"
done

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
HISTIGNORE='ls:lc:l:la:ll:lh:clear:reset'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

#
# PROMPT
#

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

# open vim help
:h() {
	vim -c ":help $*| only"
}

# prints paths to currently playing flash videos
flashdump() {
	lsof -n -P |
	awk '/FlashXX/ {
		fd="/proc/" $2 "/fd/" substr($(NF-6), 1, length($(NF-6))-1)
		if (!a[fd]++)
			print fd
	}'
}

# create a directory (if necessary) and cd into it
mkcd() {
	mkdir -p "$@" && cd "$1"
}

rustup() {
	curl -s https://static.rust-lang.org/rustup.sh |
		sh -s -- --prefix="$HOME/.local" "$@"
}

godoc() {
	command godoc "$@" |less
}

# automatically invoke sudo with apt when needed
apt() {
	local cmd skip

	# don't bother if we're root or sudo/apt isn't installed
	if [[ "$EUID" -eq 0 || ( ! -x /usr/bin/sudo && ! -x /usr/bin/apt ) ]]; then
		command apt "$@"
		return
	fi

	# determine command given to apt
	for arg in "$@"; do
		if [[ -n "$skip" ]]; then
			unset skip
			continue
		fi
		case "$arg" in
		-h*|--help|-v*|--version)
			break
			;;
		-o|-c|-t|-a)
			skip=1
			;;
		-*)
			;;
		*)
			cmd="$arg"
			break
			;;
		esac
	done

	case "$cmd" in
	install|remove|update|upgrade|full-upgrade|edit-sources)
		/usr/bin/sudo /usr/bin/apt "$@"
		;;
	*)
		command apt "$@"
		;;
	esac
}
