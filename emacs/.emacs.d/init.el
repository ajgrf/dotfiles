(require 'package)
(load-theme 'leuven)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq cursor-type 'bar)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd "<escape>") nil)
(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "o") (kbd "C-e RET i"))
(define-key god-local-mode-map (kbd "O") (kbd "C-p C-e RET i"))
(define-key god-local-mode-map (kbd "A") (kbd "C-e i"))
(define-key god-local-mode-map (kbd "I") (kbd "C-a i"))
(define-key god-local-mode-map (kbd "H") 'goto-window-top)
(define-key god-local-mode-map (kbd "M") 'goto-window-middle)
(define-key god-local-mode-map (kbd "L") 'goto-window-bottom)

(defun goto-window-top ()
  "Move point to the top of the window."
  (interactive)
  (move-to-window-line 0)
  (back-to-indentation))

(defun goto-window-bottom ()
  "Move point to the bottom of the window."
  (interactive)
  (move-to-window-line -1)
  (back-to-indentation))

;; snagged from evil-mode
(defun goto-window-middle ()
  "Move point to the middle of the window."
  (interactive)
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2))
  (back-to-indentation))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
			'box
		      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; automatically enable paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
