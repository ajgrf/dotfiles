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

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*|dvtm*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"
esac
