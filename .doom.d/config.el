;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Griffin"
      user-mail-address "a@ajgrf.com")

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
;; font string.
(let ((size (if IS-WINDOWS 11.0 12.0)))
  (setq doom-font (font-spec :family "Iosevka" :size size)
        doom-serif-font (font-spec :family "Iosevka Slab Light")
        doom-variable-pitch-font (font-spec :family "Iosevka Aile")))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme (if window-system 'doom-flatwhite 'doom-miramare))

;; Configure modus-themes:
(progn
  ;; Set variables for both themes.
  (defmacro modus-themes-format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))

  (dolist (theme '("operandi" "vivendi"))
    (modus-themes-format-sexp
     (setq modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-prompts 'subtle
           modus-%1$s-theme-completions 'opinionated
           modus-%1$s-theme-fringes 'subtle
           modus-%1$s-theme-org-blocks 'greyscale
           modus-%1$s-theme-headings '((t . section))
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-variable-pitch-headings t)
     theme)))

;; Rainbow delimiters can't be safely disabled in Doom, so just neuter it.
(custom-set-faces!
  '((rainbow-delimiters-depth-1-face rainbow-delimiters-depth-2-face
     rainbow-delimiters-depth-3-face rainbow-delimiters-depth-4-face
     rainbow-delimiters-depth-5-face rainbow-delimiters-depth-6-face
     rainbow-delimiters-depth-7-face rainbow-delimiters-depth-8-face
     rainbow-delimiters-depth-9-face)
    :foreground nil :background nil :weight normal))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Remove Doom branding from title bar.
(setq frame-title-format "%b - GNU Emacs"
      icon-title-format frame-title-format)

;; Set initial window geometry to 90x50 characters.
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(height . 50))

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
      (:prefix ("o" . "open")
       :desc "Calculator"           "c"  #'calc
       (:when (featurep! :app rss)
        :desc "Feed Reader"        "F" #'elfeed))

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
      image-dired-external-viewermage nil)

;;; :app rss
(when (featurep! :app rss)
  (require 'xdg)
  (setq elfeed-enclosure-default-dir (or (xdg-user-dir "DOWNLOAD")
                                         "~/Downloads")
        rmh-elfeed-org-files (list "links.org"))

  (after! elfeed
    (setq elfeed-search-filter "@1-month-ago +unread "))

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

;;; :editor format
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            ledger-mode))    ; sorting mangles my file

;;; :lang cc
(add-hook! c-mode
  (setq indent-tabs-mode t)
  (add-to-list 'c-default-style '(c-mode . "linux")))

;;; :lang javascript
(setq js-indent-level 2
      json-reformat:indent-width 2
      typescript-indent-level 2)

;;; :lang ledger
(when (featurep! :lang ledger)
  (setq ledger-clear-whole-transactions nil
        ledger-post-amount-alignment-column 52
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
  (map! :localleader
        :map org-mode-map
        :desc "Toggle link display" "L" #'org-toggle-link-display)
  (after! org
    (setq org-agenda-files '("~/org/plan.org" "~/org/training.org")
          org-agenda-span 'day
          org-agenda-start-day nil
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
          org-fontify-done-headline nil
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

;;; :lang web
(when (featurep! :lang web)
  (setq css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode) t))

;;; vimscript
(use-package! vimrc-mode
    :mode "\\.vim\\(rc\\)?\\'")

;;; :term eshell
(when (featurep! :term eshell)
  (after! esh-mode
    (map! :map eshell-mode-map
          :n "c"         #'evil-change
          :n "C"         #'evil-change-line
          :n "d"         #'evil-delete
          :n "D"         #'evil-delete-line
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
           :desc "Scroll on output" "s" #'eshell-toggle-scroll-to-bottom-on-output)))

  (after! eshell
    (unless IS-WINDOWS
      (require 'em-tramp)
      (set-eshell-alias! "sudo" "eshell/sudo $*"))

    (set-eshell-alias!
     "backup" "ajgrf/backup"
     "date" "*date $*"
     "dot" (concat "git --git-dir=\"" dotfiles-git-dir
                   "\" --work-tree=\"" (getenv "HOME") "\" $*")
     "edit" "find-file $1"
     "feh" "feh -. -Z -B black -D -10 $*"
     "ls" "ls -A -1 $*"
     "lc" "eshell/ls -A $*"
     "l" "lc $*"
     "la" "ls -a $*"
     "ll" "ls -la $*"
     "lh" "ll -h $*"
     "mkcd" "mkdir $1 && cd $1"
     "youtube-dl" "ajgrf/youtube-dl-url $1")

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

    (add-hook! eshell-mode
      (setenv "INSIDE_EMACS" (format "%s,eshell" emacs-version)))

    (setq eshell-scroll-to-bottom-on-input nil)))

(when (or (featurep! :term shell)
          (featurep! :term eshell)
          (featurep! :term vterm))
  (defadvice! with-project-root (orig-fn &rest args)
    "Open shells in project root when possible."
    :around '(+shell/toggle +shell/here
              +eshell/toggle +eshell/here
              +vterm/toggle +vterm/here)
    (let ((default-directory (or (projectile-project-root)
                                 default-directory)))
      (apply orig-fn args))))

;;; :term shell
(when (featurep! :term shell)
  (setq comint-completion-addsuffix '("/" . " "))
  ;; Recognize the password prompt from doas.
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^doas (.*@.*) password: \\'"))

  (when IS-WINDOWS
    ;; Prefer Powershell over cmd.exe
    (setq explicit-shell-file-name (executable-find "powershell")
          explicit-powershell.exe-args '("-NoLogo"))

    (add-hook! shell-mode
      ;; Remove input echoes
      (setq-local comint-process-echoes t)

      ;; Enable persistent history. See:
      ;; https://github.com/manzyuk/dotfiles/blob/130f86385f645f0a3a7ee6b31a479c6de2c5ce82/.emacs.d/init.el#L182
      (setq-local comint-input-ring-file-name
                  (or (getenv "HISTFILE")
                      (concat "~/AppData/Roaming/Microsoft/Windows/PowerShell/"
                              "PSReadLine/ConsoleHost_history.txt")))
      (ajgrf/turn-on-comint-history)

      ;; If the buffer associated with a process is killed, the process's
      ;; sentinel is invoked when buffer-local variables  (in particular,
      ;; `comint-input-ring-file-name' and `comint-input-ring') are gone.
      ;; Therefore try to save the history every time a buffer is killed.
      (add-hook! kill-buffer :local #'comint-write-input-ring))

    ;; Apparently, when Emacs is killed, `kill-buffer-hook' is not run
    ;; on individual buffers.  We circumvent that by adding a hook to
    ;; `kill-emacs-hook' that walks the list of all buffers and writes
    ;; the input ring (if it is available) of each buffer to a file.
    (add-hook! kill-emacs
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (comint-write-input-ring)))
            (buffer-list))))

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

;;; :tools direnv
(setq direnv-always-show-summary nil)

;;; :tools magit
(defvar dotfiles-git-dir
  (expand-file-name "~/.dot")
  "Location of dotfiles git directory.")

(when (featurep! :tools magit)
  (setq forge-topic-list-limit -5)

  (when IS-WINDOWS
    (setenv "SSH_ASKPASS" "git-gui--askpass"))

  (defadvice! with-dotfiles-git-dir (orig-fn &optional directory cache)
    "Support separate git directory for dotfiles in home."
    :around 'magit-status
    (let* ((git-dir-arg (concat "--git-dir=" dotfiles-git-dir))
           (cache (if (member git-dir-arg magit-git-global-arguments)
                      nil
                    cache)))
      (if (string= directory "~/")
          (add-to-list 'magit-git-global-arguments git-dir-arg)
        (setq magit-git-global-arguments
              (remove git-dir-arg magit-git-global-arguments)))
      (apply orig-fn (list directory cache)))))

;;; :tools pdf
(when (featurep! :tools pdf)
  (add-hook! 'pdf-view-mode-hook
    (pdf-view-auto-slice-minor-mode 1))

  (map! :map pdf-view-mode-map
        :n "J" #'pdf-view-next-page
        :n "K" #'pdf-view-previous-page
        :n "<tab>" #'pdf-outline

        :localleader
        "t" #'pdf-view-midnight-minor-mode))

;;; :ui popup
(when (featurep! :ui popup)
  (set-popup-rules!
    '(("^\\*Ledger Report" :size 25)
      ("^\\*youtube-dl"       :vslot -2)
      ("^\\*youtube-dl\\*<2>" :vslot -3)
      ("^\\*youtube-dl\\*<3>" :vslot -4)
      ("^\\*youtube-dl\\*<4>" :vslot -5)
      ("^\\*youtube-dl\\*<5>" :vslot -6)
      ("^\\*kopia" :vslot -7))))

;;; shrface
(with-eval-after-load 'shr
  (require 'shrface)
  (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
  (shrface-trial) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
  (setq shrface-href-versatile t) ; enable versatile URL faces support
                                  ; (http/https/ftp/file/mailto/other), if
                                  ; `shrface-href-versatile' is nil, default
                                  ; face `shrface-href-face' would be used.
  (setq shrface-toggle-bullets nil) ; Set t if you do not like headline bullets

  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode))

  ;; nov support
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))) ; reset nov-shr-rendering-functions, in case of the list get bigger and bigger
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode))

  ;; mu4e support
  (with-eval-after-load 'mu4e
    (add-hook 'mu4e-view-mode-hook 'shrface-mode)))

;;; Backups
(defvar ajgrf/backup-timer-enabled nil
  "Run regular system backups if non-nil.

This is set by `ajgrf/backup', so running that manually will enable the
timer on a new system.")

(when ajgrf/backup-timer-enabled
  ;; Emacs is my operating system now.
  (run-at-time t (* 2 60 60) #'ajgrf/backup))


;; Local Variables:
;; outline-regexp: ";;;"
;; eval: (outline-hide-body)
;; End:
