;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(when IS-WSL
  (package! alert-toast
    :recipe (:host github :repo "gkowzan/alert-toast")
    :pin "9b34b05650"))                           ; March 2021
(package! circadian :pin "414127acad")            ; October 2018
(package! elfeed-protocol :pin "4a59e26216")      ; 0.8.0
(package! eshell-did-you-mean :disable t)
(package! kbd-mode
  :recipe (:host github :repo "slotThe/kbd-mode")
  :pin "93bdd2300b")                              ; February 2021
(package! minions :pin "36d39bd25a")              ; v0.3.4
(package! modus-themes :pin "15c973f716")         ; 1.4.0
(package! org-multi-clock
  :recipe (:host gitlab :repo "OlMon/org-multi-clock")
  :pin "cda91b2823")                              ; April 2021
(if (file-exists-p "~/src/parchment")
    (package! parchment-theme :recipe (:local-repo "~/src/parchment"))
  (package! parchment-theme))
;; Nix profile includes pre-built pdf-tools:
(when (file-exists-p "/nix/store")
  (package! pdf-tools :recipe (:type built-in)))
(package! shrface :pin "3dc6b980a4")              ; 2.6.3
(when IS-WINDOWS
  (package! ssh-agency :pin "a5377e4317"))        ; 0.4.1
(package! vimrc-mode :pin "13bc150a87")           ; November 2018
