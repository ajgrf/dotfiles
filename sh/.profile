if test -e "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"
fi

export EDITOR="nvim"
export ENV="$HOME/.shinit"
export GOBIN="$HOME/.local/bin"
export GOPATH="${XDG_CACHE_HOME:-$HOME/.cache}/go:$HOME"
export HOSTNAME="${HOSTNAME:-$(hostname)}"
export LEDGER_FILE="${XDG_DOCUMENTS_DIR:-$HOME/Documents}/Finance/all.ledger"
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export LESSHISTFILE=-
export MANWIDTH=80
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	export TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
	export XDG_DATA_DIRS="$HOME/.nix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
fi