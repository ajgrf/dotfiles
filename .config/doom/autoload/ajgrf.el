;;; $DOOMDIR/autoload/ajgrf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ajgrf/dotfiles-magit-status ()
  "Call `magit-status' on the dotfiles vcsh repository."
  (interactive)
  (magit-status "~/"))

(defun ajgrf/kill-buffer-sentinel (process signal)
  "Sentinel to kill buffer when its process exits."
  (and (memq (process-status process) '(exit signal))
       (buffer-live-p (process-buffer process))
       (evil-delete-buffer (process-buffer process))))

(defun ajgrf/run-command-in-buffer (name args)
  (let* ((buffer-name (generate-new-buffer-name (concat "*" name "*")))
         (new-buffer
          (apply 'make-comint-in-buffer name buffer-name name nil args))
         (proc (get-buffer-process new-buffer)))
    (set-process-sentinel proc #'ajgrf/kill-buffer-sentinel)
    (switch-to-buffer-other-window new-buffer)))

;;;###autoload
(defun ajgrf/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL.  If URL is nil, use URL at point."
  (interactive)
  (let ((url (or url (thing-at-point-url-at-point))))
    (ajgrf/run-command-in-buffer "youtube-dl" (list url))))

;;;###autoload
(defun ajgrf/get-org-files ()
  (directory-files org-directory t "\.org$"))

;;;###autoload
(defun ajgrf/find-plan-file ()
  (interactive)
  (find-file-existing "~/org/plan.org"))

;;;###autoload
(defun ajgrf/mu4e-inbox ()
  (interactive)
  (require 'mu4e)
  (mu4e~headers-jump-to-maildir "/Inbox"))
