if test -z "$ENV"; then
	. "$HOME/.profile"
	. "${ENV:-$HOME/.shinit}"
fi

HISTFILE="$HOME/.ksh_history"
