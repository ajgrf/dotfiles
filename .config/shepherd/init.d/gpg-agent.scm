(define gpg-agent
  (make <service>
    #:provides '(gpg-agent)
    #:docstring "Secret key management for GnuPG"
    #:start (make-system-constructor
             "gpg-agent --daemon --pinentry-program "
             (getenv "GUIX_EXTRA_PROFILES") "/profile/profile/bin/pinentry-gtk-2")
    #:stop (make-system-destructor
            "gpg-connect-agent killagent /bye")))
(register-services gpg-agent)

(start gpg-agent)
