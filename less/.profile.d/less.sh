# make less more friendly for non-text input files, see lesspipe(1)
type lesspipe >/dev/null 2>&1 && eval "$(SHELL=/bin/sh lesspipe)"
# disable less history file
export LESSHISTFILE=/dev/null
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
