if test -e "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"
fi

export APL_LIB_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/gnu-apl"
export BROWSER="firefox"
export EDITOR="nvim"
export ENV="$HOME/.shinit"
export GOBIN="$HOME/.local/bin"
export GOPATH="${XDG_CACHE_HOME:-$HOME/.cache}/go:$HOME"
export HOSTNAME="${HOSTNAME:-$(hostname)}"
export LEDGER_FILE="${XDG_DOCUMENTS_DIR:-$HOME/Documents}/Finance/all.ledger"
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export LESSHISTFILE=-
export MANPAGER=less
export MANWIDTH=80
export MPV_HOME="$HOME/lib/mpv"
export NO_COLOR=1
export PLAN9="$HOME/.local/lib/plan9"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH:$PLAN9/bin"
export STEWPKGS="$HOME/src/github.com/ajgrf/stewpkgs"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	export TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
	export XDG_DATA_DIRS="$HOME/.nix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
fi
