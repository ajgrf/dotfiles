# Find custom Guix packages
export GUIX_PACKAGE_PATH="$HOME/src/myguix:$HOME/src/nonguix"

# Configure Guix on foreign distros
if ! test -d /run/current-system; then
	export GUIX_PROFILE="$HOME/.guix-profile"

	# Use Guix locale files for Guix packages
	export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"

	# Enable SSL support for Guix packages
	export SSL_CERT_DIR="$GUIX_PROFILE/etc/ssl/certs"
	export SSL_CERT_FILE="$GUIX_PROFILE/etc/ssl/certs/ca-certificates.crt"
	export GIT_SSL_CAINFO="$SSL_CERT_FILE"

	# Source any other environment variables Guix needs
	if test -r "$GUIX_PROFILE/etc/profile"; then
		. "$GUIX_PROFILE/etc/profile"
	fi

	unset GUIX_PROFILE
fi
