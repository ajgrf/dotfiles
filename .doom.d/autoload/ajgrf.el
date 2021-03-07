;;; $DOOMDIR/autoload/ajgrf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ajgrf/dotfiles-magit-status ()
  "Call `magit-status' on the dotfiles repository."
  (interactive)
  (magit-status "~/"))

(defun ajgrf/kill-buffer-sentinel (process signal)
  "Sentinel to kill buffer when its process exits."
  (let* ((buffer (process-buffer process))
         (wins (get-buffer-window-list buffer nil t)))
    (and (memq (process-status process) '(exit signal))
         (buffer-live-p buffer)
         (kill-buffer buffer)
         (mapc #'(lambda (w)
                   (condition-case nil
                       (delete-window w)
                     (error nil)))
               wins))))

(defun ajgrf/run-command-in-buffer (name args)
  (let* ((buffer-name (generate-new-buffer-name (concat "*" name "*")))
         (new-buffer
          (apply 'make-comint-in-buffer name buffer-name name nil args))
         (proc (get-buffer-process new-buffer)))
    (set-process-sentinel proc #'ajgrf/kill-buffer-sentinel)
    (with-current-buffer new-buffer
      (setq-local truncate-lines nil))
    (display-buffer new-buffer)))

;;;###autoload
(defun ajgrf/youtube-dl-url (&optional args)
  "Run 'youtube-dl' with ARGS.  If ARGS is nil, use URL at point."
  (interactive)
  (require 'xdg)
  (let ((default-directory (or (xdg-user-dir "DESKTOP")
                               "~/Desktop"))
        (args (or args
                  (list (thing-at-point-url-at-point)))))
    (ajgrf/run-command-in-buffer "youtube-dl" args)))

;;;###autoload
(defun ajgrf/get-org-files ()
  (directory-files org-directory t "\.org$"))

;;;###autoload
(defun ajgrf/find-plan-file ()
  (interactive)
  (find-file-existing (concat org-directory "/plan.org")))

(defun ajgrf/comint-write-history-on-exit (process event)
  "Sentinel to write history file when its process exits."
  (and (memq (process-status process) '(exit signal))
       (buffer-live-p (process-buffer process))
       (comint-write-input-ring)))

;;;###autoload
(defun ajgrf/turn-on-comint-history ()
  "Enable persistent history in the current comint session."
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (set-process-sentinel process #'ajgrf/comint-write-history-on-exit))))
