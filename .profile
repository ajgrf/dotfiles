export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
if test -e "$XDG_CONFIG_HOME/user-dirs.dirs"; then
	. "$XDG_CONFIG_HOME/user-dirs.dirs"
fi

export WORKMAN=1

export ANDROID_SDK_HOME="$XDG_DATA_HOME/android"
export APL_LIB_ROOT="$XDG_DATA_HOME/gnu-apl"
export BROWSER="plumb"
export DEBFULLNAME="Alex Griffin"
export EDITOR="nvim"
export EMAIL="a@ajgrf.com"
export ENV="$HOME/.shinit"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOBIN="$HOME/.local/bin"
export GOPATH="$XDG_CACHE_HOME/go"
export HOSTNAME="${HOSTNAME:-$(hostname)}"
export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export LEDGER_FILE="${XDG_DOCUMENTS_DIR:-$HOME/Documents}/Finance/all.ledger"
export LESS="--ignore-case --no-init --quit-if-one-screen --RAW-CONTROL-CHARS "
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export MANPAGER=less
export MANWIDTH=80
export MBLAZE="$XDG_CONFIG_HOME/mblaze"
export MU_HOME="$XDG_DATA_HOME/mu"
export NO_COLOR=1
export PLAN9="$HOME/.local/lib/plan9"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH:$PLAN9/bin"
export STACK_ROOT="$XDG_DATA_HOME/stack"
export STEWPKGS="$HOME/src/stewpkgs"
export TERMINFO_DIRS="$HOME/.local/share/terminfo"
export VCSH_GITIGNORE=none
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/init.vim" | source $MYVIMRC'
export WEECHAT_HOME="$XDG_DATA_HOME/weechat"
export XAUTHORITY="$XDG_CACHE_HOME/Xauthority"

if test "$WORKMAN"; then
	export LESSKEY="$XDG_CONFIG_HOME/less/workman.less"
	ln -sf workman.keys "$XDG_CONFIG_HOME/ncmpc/keys"
	ln -sf workman.zathurarc "$XDG_CONFIG_HOME/zathura/zathurarc"
else
	rm -f "$XDG_CONFIG_HOME/ncmpc/keys"
	ln -sf general.zathurarc "$XDG_CONFIG_HOME/zathura/zathurarc"
fi

unset SSH_AGENT_PID
if test "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$; then
	export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	export TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
	export XDG_DATA_DIRS="$HOME/.nix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
fi
