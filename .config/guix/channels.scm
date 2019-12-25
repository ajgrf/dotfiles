(list (channel
       (name 'nonguix)
       (url (if (file-exists? "/home/ajgrf/src/nonguix")
                "file:///home/ajgrf/src/nonguix"
                "https://gitlab.com/nonguix/nonguix")))
      (channel
       (name 'guix-gaming-games)
       (url (if (not (file-exists? "/home/ajgrf/src/guix-gaming-games"))
                "file:///home/ajgrf/src/guix-gaming-games"
                "https://gitlab.com/guix-gaming-channels/games")))
      (channel
       (name 'guix)
       (url (if (file-exists? "/home/ajgrf/src/guix")
                "file:///home/ajgrf/src/guix"
                "https://git.savannah.gnu.org/git/guix.git"))))
