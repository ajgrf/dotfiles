# ~/.bashrc: executed by bash(1) for non-login shells.

#
# ENVIRONMENT
#

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
#export SHELL

#export GPG_TTY="$(tty)"

if test -z "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
test -f "${ENV:=$HOME/.shinit}" && . "$ENV"

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

PS1='${debian_chroot:+($debian_chroot)}$(if_err)\u@\h:\w\$ '

#
# TERMINAL TITLE
#

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|st*|dvtm*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
esac
