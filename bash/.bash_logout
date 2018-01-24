# when leaving the console clear the screen to increase privacy
if test "$SHLVL" = 1; then
	test -x /usr/bin/clear_console && /usr/bin/clear_console -q
fi
