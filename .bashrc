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

vterm_printf() {
	if [ -n "$TMUX" ]; then
		# Tell tmux to pass the escape sequences through
		# (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
		printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
		# GNU screen (screen, screen-256color, screen-256color-bce)
		printf "\eP\e]%s\007\e\\" "$1"
	else
		printf "\e]%s\e\\" "$1"
	fi
}

vterm_prompt_end() {
	vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"      # Set title
	PS1="\[\e[1;34m\]$PS1\[\e[m\]"      # Color the prompt
	PS1="$PS1"'\[$(vterm_prompt_end)\]' # vterm directory- & prompt-tracking
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
if type direnv >/dev/null 2>&1; then
	eval "$(direnv hook bash)"
fi
