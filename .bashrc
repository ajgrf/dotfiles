if ! test -f "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
. "${ENV:-$HOME/.shinit}"

shopt -s histappend
HISTCONTROL=ignoreboth

# update LINES and COLUMNS after each command
shopt -s checkwinsize

case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"
esac
