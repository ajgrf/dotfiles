(list (channel
       (name 'nonguix)
       (url (if (file-exists? "/home/ajgrf/src/nonguix")
                "file:///home/ajgrf/src/nonguix"
                "https://gitlab.com/nonguix/nonguix"))
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc" ; 2020-07-06
         (openpgp-fingerprint                       ; Alex Griffin
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix-gaming-games)
       (url (if (file-exists? "/home/ajgrf/src/guix-gaming-games")
                "file:///home/ajgrf/src/guix-gaming-games"
                "https://gitlab.com/guix-gaming-channels/games"))
       (introduction
        (make-channel-introduction
         "c23d64f1b8cc086659f8781b27ab6c7314c5cca5" ; 2020-07-07
         (openpgp-fingerprint                       ; Pierre Neidhardt
          "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
      (channel
       (name 'guix)
       (url (if (file-exists? "/home/ajgrf/src/guix")
                "file:///home/ajgrf/src/guix"
                "https://git.savannah.gnu.org/git/guix.git"))
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad" ; 2020-05-26
         (openpgp-fingerprint                       ; Marius Bakke
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
