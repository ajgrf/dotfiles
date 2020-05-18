(define mongo
  (make <service>
    #:provides '(mongo mongod mongodb)
    #:docstring "Document-based distributed database"
    #:start (make-forkexec-constructor
             '("mongod"
               "--dbpath=/home/ajgrf/.local/share/mongodb"
               "--nojournal"
               "--rest"))
    #:stop (make-kill-destructor)))
(register-services mongo)
