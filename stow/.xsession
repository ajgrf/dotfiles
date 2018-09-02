#!/bin/sh

for file in /etc/profile "$HOME/.profile" "$HOME/.xinitrc"; do
	if test -f "$file"; then
		. "$file"
	fi
done
