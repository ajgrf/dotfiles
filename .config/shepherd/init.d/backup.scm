(define backup
  (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                              (string-append (getenv "HOME") "/.config")))
         (job-file (string-append xdg-config-home "/cron/backup.scm")))
    (make <service>
      #:provides '(backup)
      #:docstring "Daily backups"
      #:start (make-forkexec-constructor
               `("mcron" ,job-file))
      #:stop (make-kill-destructor))))
(register-services backup)

(let ((PATH (parse-path (getenv "PATH"))))
  (unless (search-path PATH "systemctl")
    (start backup)))
