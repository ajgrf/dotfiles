(register-services

 (make <service>
   #:provides '(emacs)
   #:start (make-system-constructor
            "emacs --daemon")
   #:stop (make-system-destructor
           "emacsclient --eval '(kill-emacs)'"))

 (make <service>
   #:provides '(gpg-agent)
   #:start (make-system-constructor
            "gpg-agent --daemon")
   #:stop (make-system-destructor
           "gpg-connect-agent killagent /bye"))

 (make <service>
   #:provides '(mcron cron)
   #:start (make-forkexec-constructor
            '("mcron"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(mpd)
   #:start (make-forkexec-constructor
            '("mpd" "--no-daemon"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(pulseaudio-dlna)
   #:start (make-forkexec-constructor
            '("pulseaudio-dlna"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(redshift)
   #:start (make-forkexec-constructor
            '("redshift"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(syncthing)
   #:start (make-forkexec-constructor
            '("syncthing" "-no-browser"))
   #:stop (make-kill-destructor)
   #:actions (make-actions
              (open (lambda (_) (system* "syncthing" "-browser-only"))))))

(for-each start '(gpg-agent mcron mpd redshift syncthing))
