;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Griffin"
      user-mail-address "a@ajgrf.com")

;; Focus follows mouse.
(setq mouse-autoselect-window t)

;;; Appearance

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Go Mono" :size 11.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(use-package! parchment-theme
  :config
  (add-to-list '+doom-solaire-themes '(parchment . t))
  (setq doom-theme 'parchment
        parchment-add-mode-hooks t
        parchment-want-modify-tty-colors t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Remove Doom branding from title bar.
(setq frame-title-format "%b - GNU Emacs"
      icon-title-format frame-title-format)

;; Set initial window geometry to 80x43 characters.
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 43))

;; List minor modes behind a menu in the mode-line.
(use-package! minions
  :config
  (setq doom-modeline-minor-modes t
        minions-direct '(emms))
  (minions-mode 1))

;;; Keybindings

;; Set leader keys.
(setq doom-leader-key "SPC")
(setq doom-leader-alt-key "C-SPC")
(setq doom-localleader-key "SPC m")
(setq doom-localleader-alt-key "C-SPC m")

;; The comma key also works as the local leader.
(map! :nvm ","   (general-simulate-key "SPC m")
      :ei  "C-," (general-simulate-key "C-SPC m")

      :n   "gY"  #'ajgrf/youtube-dl-url

      :leader
      (:prefix ("a" . "app")
        :desc "Calculator"           "c"  #'calc
        (:when (fboundp 'guix-popup)
          :desc "Guix popup"         "g"  #'guix-popup)
        (:when (featurep! :app rss)
          :desc "Feed Reader"        "f" #'elfeed)
        (:when (featurep! :email mu4e)
          :desc "Email inbox"        "m" #'ajgrf/mu4e-inbox))

      (:when (featurep! :tools magit)
        (:prefix ("g" . "git")
          :desc "Dotfiles status"    "d" #'ajgrf/dotfiles-magit-status))

      (:when (featurep! :lang org)
        (:prefix ("n" . "notes")
          (:desc "Open plan"         "p" #'ajgrf/find-plan-file)))

      (:prefix ("t" . "toggle")
        :desc "Auto Fill"            "F" #'auto-fill-mode
        :desc "Truncate lines"       "t" #'toggle-truncate-lines
        :desc "Visible whitespace"   "w" #'whitespace-mode

        (:prefix ("h" . "highlight")
          :desc "Current line"       "h" #'hl-line-mode
          :desc "Highlight regexp"   "r" #'highlight-regexp
          :desc "Unhighlight regexp" "u" #'unhighlight-regexp
          :desc "Matching lines"     "l" #'highlight-lines-matching-regexp
          :desc "Hi Lock mode"       "U" #'hi-lock-mode)))

;;; File Management
(setq delete-by-moving-to-trash t
      dired-dwim-target t
      dired-listing-switches "-alh"
      image-dired-external-viewermage nil)
(setq dired-guess-shell-alist-user
      '(("\\.info\\.json$" "youtube-dl --load-info-json")
        ("\\.(avi|mkv|mp4|webm)$" "mpv -fs")
        ("\\.(flac|m4a|mp3|ogg|opus)$" "mpv")
        ("\\.jpg$" "feh --cycle-once -dFZD-10 *")))

(add-to-list 'directory-abbrev-alist
             '("^/egnyte" . "/davs:focusengineering.egnyte.com:/webdav/Shared"))

(after! tramp
  ;; Add TRAMP method to integrate Magit with vcsh.
  ;; https://github.com/magit/magit/issues/2939
  (add-to-list 'tramp-methods
               '("vcsh"
                 (tramp-login-program "vcsh")
                 (tramp-login-args (("enter") ("%h")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))

  ;; Get TRAMP $PATH from "remote" profile
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; :app rss
(when (featurep! :app rss)
  (after! elfeed
    (setq elfeed-db-directory "~/.local/share/elfeed"
          elfeed-enclosure-default-dir "~/tmp/"
          elfeed-search-filter "@1-month-ago +unread "
          rmh-elfeed-org-files (list (concat org-directory "/links.org"))))

  (map! :map elfeed-search-mode-map
        :n "gr" #'elfeed-search-update--force
        :n "gR" #'elfeed-search-fetch

        :map elfeed-show-mode-map
        :n "gr" #'elfeed-show-refresh)

  (add-hook! 'elfeed-new-entry-hook
    (elfeed-make-tagger :feed-title "LWN\\.net"
                        :entry-title '("Kernel prepatch"
                                       "Security-updates"
                                       "Weekly Edition")
                        :remove 'unread)
    (elfeed-make-tagger :feed-title "Slate Star Codex"
                        :entry-title '("Link" "OT" "Thread"
                                       "Highlights")
                        :remove 'unread)
    (elfeed-make-tagger :feed-title "Barbell Logic Channel"
                        :entry-title "^#[0-9]"
                        :remove 'unread)))

;;; :completion ivy
(setq ivy-magic-tilde nil
      counsel-projectile-switch-project-action 'dired)

;;; :email mu4e
(when (featurep! :email mu4e)
  (after! mu4e
    (setq mu4e-maildir          "~/mail"
          mu4e-attachment-dir   "~"
          mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"))

  (set-email-account! "a@ajgrf.com"
    '((mu4e-sent-folder   "/Sent Items")
      (mu4e-drafts-folder "/Drafts")
      (mu4e-trash-folder  "/Trash")))

  ;; Support links to mu4e messages from Org.
  (use-package! org-mu4e
    :after mu4e
    :config
    (setq org-mu4e-link-query-in-headers-mode t))

  ;; Show email threads in a unified conversation view.
  (use-package! mu4e-conversation
    :after mu4e
    :config
    (global-mu4e-conversation-mode)))

;;; :lang cc
(add-hook! c-mode
  (setq indent-tabs-mode t)
  (add-to-list 'c-default-style '(c-mode . "linux")))

;;; :lang ledger
(when (featurep! :lang ledger)
  (setq ledger-post-amount-alignment-column 52
        ledger-reconcile-buffer-line-format "%(date)s  %-30(payee)s %-25(account)s %10(amount)s\n"
        ledger-reconcile-buffer-account-max-chars 25
        ledger-reconcile-buffer-payee-max-chars 30)

  ;; Don't reindent previous line when inserting newline.
  (setq-hook! ledger-mode electric-indent-inhibit t)

  (map! :map ledger-mode-map
        :nm "=" (general-key-dispatch 'evil-indent "=" 'ledger-post-align-dwim)
        :v  "=" #'evil-indent

        :map ledger-reconcile-mode-map
        :n "a"  #'ledger-reconcile-add
        :n "c"  #'ledger-reconcile-toggle
        :n "d"  #'ledger-reconcile-delete
        :n "t"  #'ledger-reconcile-change-target
        :n "gr" #'ledger-reconcile-refresh
        :n "q"  #'ledger-reconcile-quit
        :n "ZQ" #'ledger-reconcile-quit
        :n "ZZ" #'ledger-reconcile-finish

        :map ledger-occur-mode-map
        :nvm "q" #'ledger-occur-mode

        :localleader
        :map ledger-mode-map
        "f" #'ledger-occur

        :map ledger-reconcile-mode-map
        "," #'ledger-reconcile-toggle
        "t" #'ledger-reconcile-change-target
        "RET" #'ledger-reconcile-finish))

;;; :lang org
(setq org-directory "~/org/")
(when (featurep! :lang org)
  (after! org
    (setq org-agenda-files '("~/org/plan.org" "~/org/training.org")
          org-agenda-span 'day
          org-agenda-timegrid-use-ampm t
          org-agenda-todo-ignore-scheduled t
          org-capture-templates
          '(("t" "Task" entry (file+headline "~/org/plan.org" "Tasks")
             "* TODO %?\n %i\n  %a\n")
            ("a" "Appointment" entry (file+headline "~/org/plan.org" "Calendar")
             "* %?\n %i\n  %a\n")
            ("f" "FOCUS Task" entry (file+headline "~/org/plan.org" "FOCUS")
             "* TODO %?\n %i\n  %a\n"))
          org-default-notes-file "~/org/inbox.org"
          org-image-actual-width nil
          org-link-abbrev-alist '(("attach" . org-attach-expand-link))
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-targets '((ajgrf/get-org-files :maxlevel . 3))
          org-refile-use-outline-path 'file
          org-return-follows-link t
          org-startup-folded 'showall
          org-startup-with-inline-images t
          org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "DONE(d!)"))
          holiday-bahai-holidays nil
          holiday-hebrew-holidays nil
          holiday-islamic-holidays nil
          holiday-oriental-holidays nil
          holiday-other-holidays '((holiday-fixed 5 5 "Cinco de Mayo")))
    (add-to-list 'org-modules 'org-attach)
    (add-to-list 'org-modules 'org-depend)
    (add-to-list 'org-modules 'org-habit)))

;;; :lang scheme
(when (featurep! :lang scheme)
  (setq geiser-default-implementation 'guile)
  ;; Open files with .guile file extension in scheme-mode.
  (add-to-list 'auto-mode-alist '("\\.guile\\'" . scheme-mode) t))

;;; :lang sh
(add-to-list 'auto-mode-alist '("\\.shinit\\'" . sh-mode) t)
(setq-hook! sh-mode
  ;; Configure shell script indentation style to match shfmt.
  indent-tabs-mode t
  tab-width 4
  sh-basic-offset tab-width
  sh-indent-after-continuation 'always
  sh-indent-for-case-alt '+
  sh-indent-for-case-label 0)

;;; vimscript
(use-package! vimrc-mode
    :mode "\\.vim\\(rc\\)?\\'")

;;; :term shell
(when (featurep! :term shell)
  ;; Use Emacs as editor.
  (when (not (daemonp))
    (add-hook 'shell-mode-hook 'with-editor-export-editor))

  (setq comint-completion-addsuffix '("/" . " "))
  ;; Recognize the password prompt from my doas alias.
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^doas (.*@.*) password: \\'"))

  (map! :map shell-mode-map
        :i "C-w" #'backward-delete-word

        :map comint-mode-map
        :mode shell-mode
        :i "SPC"       #'comint-magic-space
        :i "C-k"       #'kill-line
        :i "<prior>"   #'comint-previous-matching-input-from-input
        :i "<next>"    #'comint-next-matching-input-from-input
        :i "S-<prior>" #'scroll-down-command
        :i "S-<next>"  #'scroll-up-command

        :localleader
        :mode shell-mode
        :desc "Fetch next command"  "," #'comint-get-next-from-history
        :desc "Insert previous arg" "." #'comint-insert-previous-argument
        :desc "List recent inputs"  "l" #'comint-dynamic-list-input-ring)

  ;; Make C-w behave like bash:

  ;; https://www.emacswiki.org/emacs/BackwardDeleteWord
  (defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.
  With argument, do this that many times."
    (interactive "p")
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (point) (progn (forward-word arg) (point)))))

  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.
  With argument, do this that many times."
    (interactive "p")
    (delete-word (- arg)))

  ;; Redefine a few word characters.
  (add-hook! shell-mode
    (dolist (c '(?_ ?- ?.))
      (modify-syntax-entry c "w"))
    (modify-syntax-entry ?/ "-"))

  ;; Show =apt= progress bars in the minibuffer.
  ;; https://oremacs.com/2019/03/24/shell-apt/
  (advice-add 'ansi-color-apply-on-region :before 'ora-ansi-color-apply-on-region)

  (defun ora-ansi-color-apply-on-region (begin end)
    "Fix progress bars for e.g. apt(8).
  Display progress in the mode line instead."
    (let ((end-marker (copy-marker end))
          mb)
      (save-excursion
        (goto-char (copy-marker begin))
        (while (re-search-forward "\0337" end-marker t)
          (setq mb (match-beginning 0))
          (when (re-search-forward "\0338" end-marker t)
            (let ((progress (buffer-substring-no-properties
                             (+ mb 2) (- (point) 2))))
              (delete-region mb (point))
              (ora-apt-progress-message progress)))))))

  (defun ora-apt-progress-message (progress)
    (message
     (replace-regexp-in-string
      "%" "%%"
      (ansi-color-apply progress)))))

;;; :term eshell
(when (featurep! :term eshell)
  (after! eshell
    (require 'em-tramp)                 ; For eshell's sudo.

    (set-eshell-alias!
     "abcde" "abcde -c $XDG_CONFIG_HOME/abcde.conf"
     "date" "*date $*"
     "doas" "sudo $*"
     "dot" "vcsh dotfiles $*"
     "edit" "find-file $1"
     "feh" "feh -. -Z -B black -D -10 $*"
     "ls" "ls -A -1 $*"
     "lc" "eshell/ls -A $*"
     "l" "lc $*"
     "la" "ls -a $*"
     "ll" "ls -la $*"
     "lh" "ll -h $*"
     "mkcd" "mkdir $1 && cd $1"
     "sudo" "eshell/sudo $*")

    (setq eshell-banner-message "")
    (setq eshell-prompt-function
          (lambda ()
            (concat
             (when (not (= 0 eshell-last-command-status))
               (concat (number-to-string eshell-last-command-status) "|"))
             (abbreviate-file-name (eshell/pwd))
             (if (= (user-uid) 0) "# " "$ "))))
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

    (defun eshell-toggle-scroll-to-bottom-on-output ()
      "Toggle `eshell-scroll-to-bottom-on-output'."
      (interactive)
      (setq eshell-scroll-to-bottom-on-output
            (not eshell-scroll-to-bottom-on-output)))

    (add-hook! 'eshell-first-time-mode-hook :append
      (map! :map eshell-mode-map
            :i "C-k"       #'kill-line
            :i "C-l"       #'eshell/clear
            :i "<up>"      #'eshell-previous-input
            :i "<down>"    #'eshell-next-input
            :i "<prior>"   #'eshell-previous-matching-input-from-input
            :i "<next>"    #'eshell-next-matching-input-from-input
            :i "S-<prior>" #'scroll-down-command
            :i "S-<next>"  #'scroll-up-command

            :localleader
            (:prefix ("t" . "toggle")
              :desc "Scroll on output" "s" #'eshell-toggle-scroll-to-bottom-on-output)))))

;;; :tools direnv
(setq direnv-always-show-summary nil)

;;; :tools magit
(setq emacsql-sqlite-executable (executable-find "emacsql-sqlite")
      forge-topic-list-limit -5)

;;; :tools pdf
(when (featurep! :tools pdf)
  (setq pdf-view-midnight-colors '("#000000" . "#ffffea"))

  (add-hook! 'pdf-view-mode-hook
    (pdf-view-auto-slice-minor-mode 1)
    (pdf-view-midnight-minor-mode 1))

  (map! :map pdf-view-mode-map
        :n "j" #'evil-collection-pdf-view-next-line-or-next-page
        :n "k" #'evil-collection-pdf-view-previous-line-or-previous-page
        :n "J" #'pdf-view-next-page
        :n "K" #'pdf-view-previous-page
        :n "<tab>" #'pdf-outline

        :localleader
        "t" #'pdf-view-midnight-minor-mode)

  (after! pdf-tools
    ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
    ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-532175227
    (defun brds/pdf-set-last-viewed-bookmark ()
      (interactive)
      (when (eq major-mode 'pdf-view-mode)
        (bookmark-set (brds/pdf-generate-bookmark-name))))

    (defun brds/pdf-jump-last-viewed-bookmark ()
      (when
          (brds/pdf-has-last-viewed-bookmark)
        (bookmark-jump (brds/pdf-generate-bookmark-name))))

    (defun brds/pdf-has-last-viewed-bookmark ()
      (member (brds/pdf-generate-bookmark-name) (bookmark-all-names)))

    (defun brds/pdf-generate-bookmark-name ()
      (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

    (defun brds/pdf-set-all-last-viewed-bookmarks ()
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (brds/pdf-set-last-viewed-bookmark))))

    (add-hook 'kill-buffer-hook 'brds/pdf-set-last-viewed-bookmark)
    (add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
    (unless noninteractive  ; as `save-place-mode' does
      (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))))

;;; :ui popup
(when (featurep! :ui popup)
  (set-popup-rules!
    '(("^\\*Ledger Report" :size 25)
      ("^\\*Guix" :size 0.35))))

;;; EMMS Emacs Multimedia System
(let ((emms-print-metadata (executable-find "emms-print-metadata")))
  (when emms-print-metadata
    (map! :leader
          (:prefix ("a" . "app")
            :desc "Music player" "M" #'emms))

    (use-package! emms
      :commands emms
      :config
      (require 'emms)
      (require 'emms-info-libtag)
      (emms-all)
      (emms-history-load)
      (setq emms-info-functions '(emms-info-libtag)
            emms-info-libtag-program-name emms-print-metadata
            emms-mode-line-format "[%s]"
            emms-mode-line-mode-line-function 'emms-mode-line-playlist-current
            emms-player-list '(emms-player-mpv)
            emms-playing-time-style 'downtime
            emms-source-file-default-directory "~/music/")
      (map! :localleader
            :map emms-playlist-mode-map
            "a" #'emms-browse-by-artist
            "A" #'emms-browse-by-album
            "b" #'emms-browse-by-genre
            "d" #'emms-add-directory-tree
            "f" #'emms-add-file
            "u" #'emms-add-url
            "g" #'emms-browse-by-genre
            "y" #'emms-browse-by-year
            "c" #'emms-browse-by-composer
            "p" #'emms-browse-by-performer))))

;; Display the EMMS mode-line as a ticker to save space.
(use-package! emms-mode-line-cycle
  :config
  (setq emms-mode-line-cycle-max-width 24)
  (emms-mode-line-cycle 1))


;; Local Variables:
;; outline-regexp: ";;;"
;; eval: (outline-hide-body)
;; End:
