(use-modules (gnu packages emacs-xyz)
             (guix channels)
             (guix git-download)
             (guix inferior)
             (guix packages)
             (srfi srfi-1))

(define specs
 '("aspell"
   "aspell-dict-en"
   "brightnessctl"
   "curl"
   "dconf"
   "dconf-editor"
   "ed"
   "emacs"
   "emacs-bash-completion"
   "emacs-cider"
   "emacs-clojure-mode"
   "emacs-company"
   "emacs-counsel-dash"
   "emacs-counsel-projectile"
   "emacs-diminish"
   "emacs-editorconfig"
   "emacs-elfeed"
   "emacs-elfeed-org"
   "emacs-emmet-mode"
   "emacs-evil"
   "emacs-evil-collection"
   "emacs-evil-commentary"
   "emacs-evil-magit"
   "emacs-evil-matchit"
   "emacs-evil-smartparens"
   "emacs-evil-org"
   "emacs-evil-surround"
   "emacs-flycheck"
   "emacs-geiser"
   "emacs-general"
   "emacs-go-mode"
   "emacs-guix"
   "emacs-haskell-mode"
   "emacs-helpful"
   "emacs-htmlize"
   "emacs-ivy"
   "emacs-ledger-mode"
   "emacs-magit"
   "emacs-markdown-mode"
   "emacs-mixed-pitch"
   "emacs-mu4e-conversation"
   "emacs-nov-el"
   "emacs-org"
   "emacs-org-bullets"
   "emacs-pass"
   "emacs-pdf-tools"
   "emacs-projectile"
   "emacs-rainbow-mode"
   "emacs-restart-emacs"
   "emacs-slack"
   "emacs-smart-mode-line"
   "emacs-smartparens"
   "emacs-solaire-mode"
   "emacs-use-package"
   "emacs-vimrc-mode"
   "emacs-web-mode"
   "emacs-which-key"
   "emacs-ws-butler"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "entr"
   "feh"
   "ffmpeg"
   "file"
   "font-go"
   "font-google-noto"
   "ghc-xmonad-contrib"
   "git"
   "glibc-utf8-locales"
   "gnome-tweaks"
   "gnupg"
   "gst-plugins-bad"
   "gst-plugins-good"
   "gst-plugins-ugly"
   "guile"
   "icecat"
   "imagemagick"
   "isync"
   "ledger"
   "libnotify"
   "libreoffice"
   "mcron"
   "moreutils"
   "mpv"
   "msmtp"
   "mu"
   "mutt"
   "neovim"
   "next"
   "nss-certs"
   "openssh"
   "pass-otp"
   "password-store"
   "pinentry"
   "poppler"
   "pulseaudio-dlna"
   "rclone"
   "recutils"
   "redshift"
   "reptyr"
   "restic"
   "rsync"
   "shellcheck"
   "shepherd"
   "stow"
   "syncthing"
   "the-silver-searcher"
   "tmux"
   "transmission:gui"
   "trash-cli"
   "ungoogled-chromium"
   "vcsh"
   "vim"
   "w3m"
   "xclip"
   "xdg-utils"
   "xdotool"
   "xmonad"
   "xrdb"
   "xterm"
   "youtube-dl"))

(define-public my-emacs-emms
  (let ((commit "e70459caaadeb715116abb45ddf5e98921d46c14")
        (revision "1"))
    (package
      (inherit emacs-emms)
      (version (git-version "20190620" revision commit))
      (source
       (origin
         (inherit (package-source emacs-emms))
         (uri (string-append
               "https://git.savannah.gnu.org/cgit/emms.git/snapshot/emms-"
               commit ".tar.gz"))
         (sha256
          (base32
           "0vxqwn1mvrqkvmm3ym5wvl5kffqf430c59mj4wq4z8ci7s62c100")))))))

(define-public my-emacs-emms-mode-line-cycle
  (package
    (inherit emacs-emms-mode-line-cycle)
    (propagated-inputs `(("emms" ,my-emacs-emms)))))

(packages->manifest
 (cons*
  my-emacs-emms
  my-emacs-emms-mode-line-cycle
  (map (compose list specification->package+output) specs)))
