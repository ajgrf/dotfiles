if type feh >/dev/null 2>&1; then
	for theme in $(awk '{print $1}' ~/.config/feh/themes); do
		if ! test -L "$HOME/.local/bin/$theme"; then
			ln -s $(which feh) "$HOME/.local/bin/$theme"
		fi
	done
fi
