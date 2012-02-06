# ~/.bashrc

# prompt
PS1='\u@\h:\w\$ '

# set terminal title
case "$TERM" in
	*xterm* | rxvt* | screen)
		PROMPT_COMMAND='printf "\e]0;${USER}@${HOSTNAME}: ${PWD}\a"'
esac

# add extra bin directories in home and elsewhere to $PATH
PATH="$HOME/bin:$PATH"

# fall back to simplified chars if using chinese traditional locale
test "$LANG" = "zh_TW.utf8" && export LANGUAGE="zh_TW.utf8:zh_CN.utf8"

# history settings
shopt -s histappend
HISTFILESIZE=10000
HISTSIZE="$HISTFILESIZE"
HISTCONTROL='ignoreboth'
HISTIGNORE='ls:l:la:ll:clear:reset'

# enable programmable completion features
test -r /etc/bash_completion && . /etc/bash_completion

# change directory upon exiting mc
test -r /usr/share/mc/bin/mc.sh && . /usr/share/mc/bin/mc.sh

# make less more friendly for non-text input files, see lesspipe(1)
eval "$(lesspipe)"
# disable less history file
export LESSHISTFILE='/dev/null'

# enable color when it's available
if [ $(tput colors) -gt 1 ]; then
	# colored prompt
	yl='\[\e[33m\]' gr='\[\e[32m\]' bl='\[\e[34m\]' wh='\[\e[1;37m\]' d='\[\e[0m\]'
	PS1="${yl}\u${d}@${gr}\h${d}:${bl}\w${wh}\$${d} "
	unset yl gr bl wh d

	# colored man pages
	export LESS_TERMCAP_mb=$(printf '\e[1;37m')
	export LESS_TERMCAP_md=$(printf '\e[1;37m')
	export LESS_TERMCAP_me=$(printf '\e[0m')
	export LESS_TERMCAP_se=$(printf '\e[0m')
	export LESS_TERMCAP_so=$(printf '\e[30;106m')
	export LESS_TERMCAP_ue=$(printf '\e[0m')
	export LESS_TERMCAP_us=$(printf '\e[36m')

	# enable color support for ls
	eval "$(dircolors -b ~/.dircolors 2>/dev/null || dircolors -b)"
	alias ls='LC_COLLATE=C ls --group-directories-first --color=auto'

	# colored grep
	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
else
	# if color isn't available, use filetype indicators in ls
	alias ls='LC_COLLATE=C ls --group-directories --classify'
fi

# other aliases & functions:
alias 9='env PATH="/usr/lib/plan9/bin:$PATH"'
alias clamz='clamz --default-output-dir=\${XDG_MUSIC_DIR:-\$HOME/Music}/\${album_artist}/\${album}'
alias df='df -h'
alias du='du -h'
alias flashdump='lsof -n -P | awk '\''
	/FlashXX/ {
		fd="/proc/" $2 "/fd/" substr($(NF-6), 1, length($(NF-6))-1)
		if (!a[fd]++)
			print fd
	}'\'
alias l='ls'
alias la='ls -A'
alias ll='la -lh'
alias open='xdg-open'
alias pgrep='pgrep -l'
alias startx='exec xinit "${XINITRC:-$HOME/.xinitrc}"'
alias svi='sudo vi'

:h() {	vim --cmd ":silent help $@" --cmd "only"; }

mkcd() {
	mkdir "$1" && cd "$1"
}

mpd-wait() {
	for n in `seq "${1:-1}"`; do
		mpc current --wait >/dev/null
	done
}
