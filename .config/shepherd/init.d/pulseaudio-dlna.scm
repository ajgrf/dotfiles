(define pulseaudio-dlna
  ;; Make sure to open ports 8376/tcp, 29254/udp, and 1900/udp
  (make <service>
    #:provides '(pulseaudio-dlna)
    #:docstring "Stream audio to DLNA/UPnP and Chromecast devices"
    #:start (make-forkexec-constructor
             '("pulseaudio-dlna" "--encoder-backend" "ffmpeg"
               "--port" "8376" "--msearch-port" "29254"))
    #:stop (make-kill-destructor)))
(register-services pulseaudio-dlna)
