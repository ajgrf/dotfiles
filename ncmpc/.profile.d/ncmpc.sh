# Use Colemak bindings or not
if test -n "$COLEMAK" -a ! -r "$HOME/.ncmpc/keys"; then
	ln -s keys.colemak "$HOME/.ncmpc/keys"
elif test -w "$HOME/.lesskey"; then
	rm "$HOME/.ncmpc/keys"
fi
