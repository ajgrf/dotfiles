export SPACEMACSDIR="${XDG_CONFIG_HOME:-$HOME/.config}/spacemacs"

if ! test -d "$HOME/.emacs.d"; then
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d &
fi
