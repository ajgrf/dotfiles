(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:docstring "Peer-to-peer file synchronization"
    #:start (make-forkexec-constructor
             '("syncthing" "-no-browser"))
    #:stop (make-kill-destructor)
    #:actions (make-actions
               (open (lambda (_) (system* "syncthing" "-browser-only"))))
    #:respawn? #t))
(register-services syncthing)

(start syncthing)
