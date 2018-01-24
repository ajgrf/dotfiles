if test -e "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs"
fi

export EDITOR="nvim"
export ENV="$HOME/.shinit"
export GOBIN="$HOME/.local/bin"
export GOPATH="${XDG_CACHE_HOME:-$HOME/.cache}/go:$HOME"
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export LESSHISTFILE=-
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"
