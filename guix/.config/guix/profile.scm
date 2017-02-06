(use-package-modules admin apl attr audio backup base code databases ebook
                     emacs feh file finance fonts games gnome gnupg gnuzilla
                     gstreamer guile moreutils mpd music package-management
                     parallel password-utils rsync screen shellutils ssh
                     version-control video vim xdisorg xorg)

(define emacs-pkgs
  (list emacs
        emacs-elfeed
        emacs-org-bullets
        geiser
        magit
        paredit))

(packages->manifest
 (append
  emacs-pkgs
  (list apl
        attr
        beets
        borg
        bs1770gain
        calibre
        feh
        ffmpeg
        file
        font-dejavu
        font-google-noto
        font-wqy-zenhei
        git
        git-manpages
        gnome-mpv
        gnome-tweak-tool
        gnupg
        gst-plugins-ugly
        gst-plugins-bad
        icecat
        ledger
        mcron2
        moreutils
        mpd
        mpd-mpc
        mpv
        ncmpc
        openssh
        password-store
        parallel
        pinentry
        recutils
        redshift
        reptyr
        rsync
        screen
        stow
        the-silver-searcher
        trash-cli
        vim
        xclip
        xset
        youtube-dl
        wesnoth
        which)))
