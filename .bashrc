if ! test -f "$ENV"; then
	test -f "$HOME/.profile" && . "$HOME/.profile"
fi
. "${ENV:-$HOME/.shinit}"

shopt -s histappend
HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/bash_history"
HISTFILESIZE=20000
HISTCONTROL=ignoreboth

# update LINES and COLUMNS after each command
shopt -s checkwinsize

prompt() {
	local e="$?"  dir="${PWD}/" S='$'
	test "$e" -eq 0 && e=""
	dir="${dir/#$HOME\//\~/}"; dir="${dir%/}"
	test "$UID" -eq 0 && S='#'

        if type git >/dev/null 2>&1; then
		branch=$(git symbolic-ref --short HEAD 2>/dev/null)
	fi

	test "${#dir}" -gt 28 && dir="...${dir:(-25)}"
	echo -n "${e:+$e|}${USER}@${HOSTNAME%%.*}:${dir}${branch:+ ($branch)}$S "
}

case "$TERM" in
xterm*|rxvt*|st*|screen*|tmux*)
	PS1="\[\e]0;\u@\h: \w\a\]$PS1"
esac
