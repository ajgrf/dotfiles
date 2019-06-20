(cons (channel
       (name 'nonguix)
       (url (if (file-exists? "/home/ajgrf/src/nonguix")
                "file:///home/ajgrf/src/nonguix"
                "https://gitlab.com/nonguix/nonguix")))
      %default-channels)
