bind-key o next-window

bind-key -T copy-mode-vi n send-keys -X cursor-down
bind-key -T copy-mode-vi e send-keys -X cursor-up
bind-key -T copy-mode-vi y send-keys -X cursor-left
bind-key -T copy-mode-vi o send-keys -X cursor-right
bind-key -T copy-mode-vi k send-keys -X search-again
bind-key -T copy-mode-vi K send-keys -X search-reverse
bind-key -T copy-mode-vi N send-keys -X scroll-down
bind-key -T copy-mode-vi E send-keys -X scroll-up
bind-key -T copy-mode-vi Y send-keys -X top-line
bind-key -T copy-mode-vi O send-keys -X bottom-line
bind-key -T copy-mode-vi h send-keys -X next-word-end
bind-key -T copy-mode-vi H send-keys -X next-space-end
bind-key -T copy-mode-vi l send-keys -X other-end
bind-key -T copy-mode-vi c-n send-keys -X copy-selection

bind-key -T choice-mode n send-keys -X down
bind-key -T choice-mode e send-keys -X up
bind-key -T choice-mode c-n send-keys -X choose

bind-key -T edit-mode-vi c-n send-keys -X enter
