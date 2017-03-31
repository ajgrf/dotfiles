# ~/.bashrc: executed by bash(1) for non-login shells.

#
# ENVIRONMENT
#

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
#export SHELL

#export GPG_TTY="$(tty)"

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
xterm*|rxvt*|st*|dvtm*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
esac

#
# ALIASES, FUNCTIONS, ETC.
#

alias doch='eval sudo $(fc -ln -1)'

# some ls aliases
alias ls='LC_COLLATE=C ls --time-style=long-iso -A -1'
alias lc='LC_COLLATE=C command ls --time-style=long-iso -A'
alias l='lc -p'
alias la='ls'
alias ll='ls -la'
alias lh='ll -h'

alias ag='ag --nocolor --nogroup'
alias emacs='emacs -nw'
#alias gpg='gpg2'
alias nb='newsbeuter'
alias open='xdg-open'

# open vim help
:h() {
	vim -c ":help $*| only"
}

# create a directory (if necessary) and cd into it
mkcd() {
	mkdir -p "$@" && cd "$1"
}

ytdl() {
	local selection
	selection="$(xclip -out)"
	if [ $# -eq 0 ] && [ -n "$selection" ]; then
		youtube-dl -- "$selection"
	else
		youtube-dl "$@"
	fi
}

article-convert() {
	local title
	pushd "${1:-$HOME/tmp}" > /dev/null
	mkdir -p "$HOME/tmp/articles"
	for article in *_files; do
		title="${article%_files}"
                mv "$title" "${title}.html"
		ebook-convert "${title}.html" "$HOME/tmp/articles/${title}.mobi" &&
		trash "${title}.html" "${title}_files"
	done
	popd > /dev/null
}

# automatically invoke sudo with apt when needed
apt() {
	local cmd skip

	# don't bother if we're root
	if [ "$(id -u)" -eq 0 ]; then
		command apt "$@"
		return
	fi

	# determine command given to apt
	for arg in "$@"; do
		if [ -n "$skip" ]; then
			skip=""
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
		sudo apt "$@"
		;;
	*)
		command apt "$@"
		;;
	esac
}

guix() {
	local fmtstr guixdir guixenv url
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
	clean)
		git -C "$guixdir/latest" clean --exclude='/gnu/packages/bootstrap' -fdx
		;;
	env)
		shift
		"$guixenv" guix environment "$@"
		;;
	ls)
		for dir in $(guix build "$2"); do
			echo
			echo "$dir:"
			(cd "$dir" && find) |
				sed -e '/^\.$/d' -e 's/^\.\/\?//' |
				sort
		done | sed 1d
		;;
	make)
		if test -f "$guixdir/latest/Makefile"; then
			"$guixenv" guix environment guix -- make -C "$guixdir/latest"
		else
			guixdir="$guixdir" "$guixenv" guix environment guix -- bash -c '
				cd "$guixdir/latest" &&
				./bootstrap &&
				./configure --localstatedir=/var --sysconfdir=/etc &&
				make &&
				git checkout po/'
		fi
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
	visit)
		url=$("$guixenv" guix package --show="$2" |
			recsel -CP homepage |
			uniq)
		xdg-open "$url" 2> /dev/null
		;;
	*)
		"$guixenv" guix "$@"
		;;
	esac
}
