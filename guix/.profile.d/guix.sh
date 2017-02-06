# Configure Guix on foreign distros
if ! test -d /run/current-system; then
	export GUIX_PROFILE="$HOME/.guix-profile"
	export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
	if test -r "$GUIX_PROFILE/etc/profile"; then
		. "$GUIX_PROFILE/etc/profile"
	fi
	unset GUIX_PROFILE
fi

export GUIX_PACKAGE_PATH="$HOME/src/myguix:$HOME/src/nonguix"
