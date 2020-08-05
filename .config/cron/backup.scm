;; Attempt backup every 8 hours.
(job '(next-hour '(0 8 16))
     "tmux -f \"${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.conf\" new-session -d -t mysession \\; new-window -t mysession:0 -n backup 'backup; sleep 120'"
     "backup")
