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

# enable programmable completion
if test -f /usr/share/bash-completion/bash_completion; then
	. /usr/share/bash-completion/bash_completion;
elif test -f /etc/bash_completion; then
	 . /etc/bash_completion
elif test -f /run/current-system/profile/etc/profile.d/bash_completion.sh; then
	. /run/current-system/profile/etc/profile.d/bash_completion.sh
fi

# load direnv hook
eval "$(direnv hook bash)"
