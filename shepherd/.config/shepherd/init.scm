(register-services
 (make <service>
   #:provides '(emacs)
   #:requires '()
   #:start (make-system-constructor "emacs --daemon")
   #:stop (make-system-destructor "emacsclient --eval '(kill-emacs)'"))
 (make <service>
   #:provides '(gpg-agent)
   #:requires '()
   #:start (make-system-constructor
            "gpg-agent --daemon --pinentry-program "
              (getenv "HOME") "/.guix-profile/bin/pinentry-gtk-2")
   #:stop (make-system-destructor
           "gpg-connect-agent killagent /bye"))
 (make <service>
   #:provides '(mcron)
   #:requires '()
   #:start (make-forkexec-constructor
            '("mcron"))
   #:stop (make-kill-destructor))
 (make <service>
   #:provides '(mpd)
   #:requires '()
   #:start (make-forkexec-constructor
            '("mpd" "--no-daemon"))
   #:stop (make-kill-destructor))
 (make <service>
   #:provides '(redshift)
   #:requires '()
   #:start (make-forkexec-constructor
            '("redshift"))
   #:stop (make-kill-destructor)))

(for-each start '(gpg-agent mcron mpd redshift))
