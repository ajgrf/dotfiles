# make less more friendly for non-text input files, see lesspipe(1)
hash lesspipe 2> /dev/null && eval "$(SHELL=/bin/sh lesspipe)"
# disable less history file
export LESSHISTFILE=/dev/null
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "

# Use Colemak bindings or not
if test -n "$COLEMAK" -a ! -r "$HOME/.lesskey"; then
	lesskey
elif test -w "$HOME/.lesskey"; then
	rm "$HOME/.lesskey"
fi
