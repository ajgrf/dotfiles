(define emacs
  (make <service>
    #:provides '(emacs)
    #:docstring "Emacs server"
    #:start (make-system-constructor
             "emacs --daemon")
    #:stop (make-system-destructor
            "emacsclient --eval '(kill-emacs)'")))
(register-services emacs)
