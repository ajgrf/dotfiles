# ~/.profile: executed by the command interpreter for login shells.

export EDITOR="vim"
export TERMINFO="$HOME/.local/share/terminfo"

# Go
export GOPATH="${XDG_CACHE_HOME:-$HOME/.cache}/go:$HOME"
export GOBIN="$HOME/.local/bin"

# Rust
export RUST_SRC_PATH="$HOME/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Load profile fragments from directory
for f in ~/.profile.d/*.sh; do
	test -r "$f" && . "$f"
done
unset f
