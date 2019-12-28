(use-modules (gnu packages emacs-xyz)
             (guix git-download)
             (guix packages))

(define specs
 '("emacs"
   "emacs-auth-source-pass"
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
   "emacs-emms"
   "emacs-emms-mode-line-cycle"
   "emacs-evil"
   "emacs-evil-collection"
   "emacs-evil-commentary"
   "emacs-evil-magit"
   "emacs-evil-matchit"
   "emacs-evil-smartparens"
   "emacs-evil-org"
   "emacs-evil-surround"
   "emacs-flycheck"
   "emacs-forge"
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
   "emacs-org-contrib"
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
   "mu"))

;; Needed to prevent eshell-path-env getting out-of-sync with $PATH (#55).
(define-public my-emacs-direnv
  (let ((commit "fd0b6bbd5e3eaf6aa48bccd4a1ff3048bfb2c69b")
        (revision "1"))
    (package
      (inherit emacs-direnv)
      (version (git-version "20191016" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/wbolster/emacs-direnv.git")
                (commit commit)))
          (file-name (git-file-name "emacs-direnv" version))
          (sha256
           (base32
            "0py0if1wl61y6f55s4p8y11rjvrgx3yk2v5n1q2xl3gg7f4ra136")))))))

(packages->manifest
  (cons* my-emacs-direnv
         (map (compose list specification->package+output) specs)))
