# ~/.profile: executed by the command interpreter for login shells.

export EDITOR="vim"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"

# Go
export GOPATH="${XDG_CACHE_HOME:-$HOME/.cache}/go:$HOME"
export GOBIN="$HOME/.local/bin"

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# make less more friendly for non-text input files, see lesspipe(1)
type lesspipe >/dev/null 2>&1 && eval "$(SHELL=/bin/sh lesspipe)"
# disable less history file
export LESSHISTFILE=/dev/null
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
