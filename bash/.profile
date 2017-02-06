# ~/.profile: executed by the command interpreter for login shells.

export EDITOR="vim"

# Guix
export GUIX_PACKAGE_PATH="$HOME/src/myguix"

# Go
export GOPATH="$HOME/.local:$HOME"
export GOBIN="$HOME/.local/bin"
if [ -d "$HOME/.local/go" ]; then export GOROOT="$HOME/.local/go"; fi

# Python
export PYTHONPATH="/home/ajgrf/.guix-profile/lib/python2.7/site-packages"

export PATH="$HOME/bin:$HOME/.local/bin:${GOROOT:+$GOROOT/bin:}$PATH"

# Load profile fragments from directory
for f in ~/.profile.d/*.sh; do
	test -r "$f" && . "$f"
done
unset f
