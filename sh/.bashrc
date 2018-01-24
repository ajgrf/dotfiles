if test -z "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
. "${ENV:-$HOME/.shinit}"

# history settings
shopt -s histappend
HISTSIZE=10000
HISTFILESIZE=20000
HISTCONTROL=ignoreboth
HISTIGNORE='l:l[salhc]:cd:clear:reset'

# update LINES and COLUMNS after each command
shopt -s checkwinsize

# enable "**" globbing
shopt -s globstar

if_err() {
	local e=$?
	if [ $e -ne 0 ]; then
		echo -n "$e|"
	fi
}

# pretty print working directory
ppwd() {
	local dir="${PWD}/" n="${1:-255}"

	dir="${dir/#$HOME\//\~/}"; dir="${dir%/}"
	test "${#dir}" -gt "$n"  && dir="...${dir:(-$((n-3)))}"
	echo "$dir"
}

PS1='$(if_err)${debian_chroot:+($debian_chroot)}\u@\h:$(ppwd 28)\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*|dvtm*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
esac
