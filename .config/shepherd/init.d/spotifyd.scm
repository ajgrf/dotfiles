(define spotifyd
  (make <service>
    #:provides '(spotifyd spotify)
    #:docstring "Spotify client service"
    #:start (make-forkexec-constructor
             '("spotifyd" "--no-daemon"))
    #:stop (make-kill-destructor)))
(register-services spotifyd)
