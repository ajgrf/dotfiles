(list (channel
       (name 'nonguix)
       (url (if (file-exists? "/home/ajgrf/src/nonguix")
                "file:///home/ajgrf/src/nonguix"
                "https://gitlab.com/nonguix/nonguix")))
      (channel
       (name 'guix)
       (url (if (file-exists? "/home/ajgrf/src/guix")
                "file:///home/ajgrf/src/guix"
                "https://git.savannah.gnu.org/git/guix.git"))))
