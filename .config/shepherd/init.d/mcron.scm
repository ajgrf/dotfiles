(define mcron
  (make <service>
    #:provides '(mcron cron)
    #:docstring "Time-based job scheduler"
    #:start (make-forkexec-constructor
             '("mcron"))
    #:stop (make-kill-destructor)))
(register-services mcron)

(start mcron)
