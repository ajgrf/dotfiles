(use-modules (guix channels)
             (guix inferior)
             (srfi srfi-1))  ; for 'first'

(define channels
  ;; Old revision with bs1770gain@0.5.2.
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit "30696ea2b27747e047589527572ef39e60555047"))))

(define inferior
  (inferior-for-channels channels))

(define specs
  '("abcde"
    "beets"
    "ffmpeg"
    "imagemagick"))  ; for artresizer

(packages->manifest
 (cons* (first (lookup-inferior-packages inferior "bs1770gain"))
        (map (compose list specification->package+output) specs)))
