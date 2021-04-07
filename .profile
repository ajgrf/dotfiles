# Shell Variables
export ENV="$HOME/.shinit"
export CDPATH=":$HOME:$HOME/src"
export HISTSIZE=20000
export HOSTNAME="${HOSTNAME:-$(hostname)}"
export USER="${USER:-$(id -un)}"

# Preferred Programs
export EDITOR="emacsclient"
export ALTERNATE_EDITOR=""

# Miscellaneous Configuration
export DIRENV_LOG_FORMAT=
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export LS_COLORS="bd=35:cd=35:di=1;34:ex=1;32:fi=0:ln=1;36:mi=3;31:or=3;31:pi=35:so=35:*.bak=33:*~=33:*#=33"
export MANPAGER=less
export NO_COLOR=1
export RCLONE_PASSWORD_COMMAND="bw get password rclone"

# Configure Nix package manager
if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	export XDG_DATA_DIRS="$HOME/.nix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
fi

# Put my scripts and executables at the front of $PATH
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.emacs.d/bin:$PATH"

# Set up GnuPG Agent
if type gpgconf >/dev/null 2>&1; then
	unset SSH_AGENT_PID
	if test "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$; then
		export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
	fi
fi

# Load site-local profile
if test -e "$HOME/.profile.local"; then
	. "$HOME/.profile.local"
fi
