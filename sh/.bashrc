if test -z "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
. "${ENV:-$HOME/.shinit}"

shopt -s histappend
HISTFILESIZE=20000
HISTCONTROL=ignoreboth

# update LINES and COLUMNS after each command
shopt -s checkwinsize

case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"
esac

bind 'set bind-tty-special-chars off'
bind '"\C-w": unix-filename-rubout'
bind '"\e[5~": history-search-backward'
bind '"\e[6~": history-search-forward'
bind '"\C-\M-j": "\C-e |less\n"'
bind '"\C-\M-m": "\C-e |less\n"'
bind '" ": magic-space'
