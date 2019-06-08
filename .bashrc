if ! test -f "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
. "${ENV:-$HOME/.shinit}"

shopt -s histappend
HISTCONTROL=ignoreboth

# update LINES and COLUMNS after each command
shopt -s checkwinsize

# pretty print working directory for PS1. Faster implementation with bashisms.
ppwd() {
	local dir="${PWD}/" n="${1:-255}"

	dir="${dir/#$HOME\//\~/}"
	dir="${dir%/}"

	if test "${#dir}" -gt "$n"; then
		n=$(( n - 3 ))
		dir="...${dir:(-$n)}"
	fi

	echo "$dir"
}

case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"
esac
