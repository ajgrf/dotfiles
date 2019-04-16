# Important Shell Variables
export ENV="$HOME/.shinit"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export CDPATH=":$HOME:$HOME/src"
export HISTSIZE=20000
export HOSTNAME="${HOSTNAME:-$(hostname)}"

# Preferred Programs
export BROWSER="plumb"
export EDITOR="emacs -nw"

# Miscellaneous Configuration
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export MANPAGER=less
export NO_COLOR=1
export VCSH_GITIGNORE=none

# Paths
export GOBIN="$HOME/.local/bin"
export LEDGER_FILE="$HOME/docs/Finance/all.ledger"
export STEWPKGS="$HOME/src/stewpkgs"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"

# XDG Base Directories

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
if test -e "$XDG_CONFIG_HOME/user-dirs.dirs"; then
	. "$XDG_CONFIG_HOME/user-dirs.dirs"
fi

export ANDROID_SDK_HOME="$XDG_DATA_HOME/android"
export APL_LIB_ROOT="$XDG_DATA_HOME/gnu-apl"
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_CACHE_HOME/go"
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/sh_history"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export MBLAZE="$XDG_CONFIG_HOME/mblaze"
export MU_HOME="$XDG_DATA_HOME/mu"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export STACK_ROOT="$XDG_DATA_HOME/stack"
export VIMINIT='if has("eval") | let $MYVIMRC="$XDG_CONFIG_HOME/vim/init.vim" | source $MYVIMRC | endif'
export WEECHAT_HOME="$XDG_DATA_HOME/weechat"

# Set up GnuPG Agent
unset SSH_AGENT_PID
if test "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$; then
	export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# Configure Nix package manager
if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	export TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
	export XDG_DATA_DIRS="$HOME/.nix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
fi

# Load site-local profile
if test -e "$HOME/.profile.local"; then
	. "$HOME/.profile.local"
fi

# Configure Workman bindings if the WORKMAN variable is set
if test "$WORKMAN"; then
	export LESSKEY="$XDG_CONFIG_HOME/less/workman.less"
	ln -sf workman.keys "$XDG_CONFIG_HOME/ncmpc/keys"
	ln -sf workman.zathurarc "$XDG_CONFIG_HOME/zathura/zathurarc"
else
	rm -f "$XDG_CONFIG_HOME/ncmpc/keys"
	ln -sf general.zathurarc "$XDG_CONFIG_HOME/zathura/zathurarc"
fi

