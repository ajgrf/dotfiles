# Apply dconf settings
for f in "${XDG_CONFIG_HOME:-$HOME/.config}/dconf/user.d/"*.conf; do
	if type dconf >/dev/null 2>&1 && test -r "$f"; then
		dconf load / < "$f"
	fi
done
unset f
