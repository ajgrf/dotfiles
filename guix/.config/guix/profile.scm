(use-modules (guix packages)
             (guix profiles)
             (ice-9 match))

(use-package-modules admin abduco attr audio backup bittorrent certs entr code
                     compression cryptsetup databases ebook emacs file finance
                     fonts games geo gnome gnupg gnuzilla gstreamer guile
                     image-viewers linux moreutils mpd music package-management
                     parallel password-utils perl rsync screen shellutils ssh
                     text-editors version-control video vim web xdisorg xorg)

;; Redefinition of `packages->manifest` function, with added support for raw
;; store paths.
(define (packages->manifest* packages)
  "Return a list of manifest entries, one for each item listed in PACKAGES.
Elements of PACKAGES can be either package objects, package/string tuples
denoting a specific output of a package, or store paths."
  (define store-item->manifest-entry
    (@@ (guix scripts package) store-item->manifest-entry))
  (manifest
   (map (match-lambda
          ((package output)
           (package->manifest-entry package output))
          ((? package? package)
           (package->manifest-entry package))
          ((? file-exists? store-item)
           (store-item->manifest-entry store-item)))
        packages)))

(define foreign-pkgs
  (list glibc-utf8-locales
        guix
        nss-certs
        shepherd))

(define common-pkgs
  (list abduco
        emacs
        emacs-guix
        entr
        font-comic-neue
        font-go
        font-iosevka
        guile-2.0
        mcron2
        recutils
        trash-cli
        vis
        youtube-dl))

(define guixsd-pkgs
  (list aria2
        attr
        beets
        bluez
        borg
        bs1770gain
        calibre
        cryptsetup
        emacs-elfeed
        emacs-guix
        emacs-org-bullets
        feh
        ffmpeg
        file
        font-dejavu
        font-google-noto
        font-wqy-zenhei
        geiser
        git
        gnome-maps
        gnome-mpv
        gnome-tweak-tool
        gnupg
        gst-plugins-bad
        gst-plugins-ugly
        guile-json
        icecat
        jq
        ;; ledger
        "/gnu/store/wppn6nyy97spa92p1wd9pn457p330l53-ledger-3.1.1"
        lvm2
        magit
        moreutils
        mpd
        mpd-mpc
        mpv
        ncmpc
        openssh
        p7zip
        parallel
        paredit
        password-store
        perl
        ;; perl-file-rename
        "/gnu/store/miizffw02271ivf0m5m8jkqzkrldpffv-perl-file-rename-0.20"
        pinentry
        powertop
        redshift
        reptyr
        rsync
        screen
        stow
        the-silver-searcher
        vim
        wesnoth
        which
        xclip
        xset))

(packages->manifest*
 (append (if (file-exists? "/run/current-system")
             guixsd-pkgs
             foreign-pkgs)
         common-pkgs))
