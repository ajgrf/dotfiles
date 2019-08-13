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
            "gpg-agent --daemon --pinentry-program "
            (getenv "HOME") "/.guix-profile/bin/pinentry-gtk-2")
   #:stop (make-system-destructor
           "gpg-connect-agent killagent /bye"))

 (make <service>
   #:provides '(mcron cron)
   #:start (make-forkexec-constructor
            '("mcron"))
   #:stop (make-kill-destructor))

 ;; Make sure to open ports 8376/tcp, 29254/udp, and 1900/udp
 (make <service>
   #:provides '(pulseaudio-dlna)
   #:start (make-forkexec-constructor
            '("pulseaudio-dlna" "--encoder-backend" "ffmpeg"
              "--port" "8376" "--msearch-port" "29254"))
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

(for-each start '(emacs gpg-agent mcron redshift syncthing))
