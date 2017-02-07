if type feh >/dev/null 2>&1; then
	for theme in $(awk '{print $1}' ~/.config/feh/themes); do
		ln -s $(which feh) "$HOME/.local/bin/$theme"
	done
fi
