(use-package-modules admin backup base code databases ebook emacs feh file
                     finance fonts games gnome gnupg gnuzilla gstreamer guile
                     moreutils mpd package-management parallel password-utils
                     rsync ssh version-control video vim xdisorg xorg)

(use-modules (trash))

(packages->manifest
 (list borg
       calibre
       emacs
       feh
       file
       font-dejavu
       font-google-noto
       font-wqy-zenhei
       geiser
       git
       gnome-tweak-tool
       gst-plugins-ugly
       gst-plugins-bad
       icecat
       ledger
       magit
       mcron2
       moreutils
       mpd
       mpd-mpc
       mpv
       ncmpc
       openssh
       parallel
       paredit
       recutils
       redshift
       rsync
       setxkbmap
       stow
       the-silver-searcher
       trash-cli
       vim
       xcape
       xset
       youtube-dl
       zenity

       password-store
       gnupg
       pinentry
       pwgen
       tree
       wesnoth
       which
       xclip))
