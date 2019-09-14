(in-package :next)

(add-to-default-list 'blocker-mode   'buffer 'default-modes)
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

(setf (get-default 'window 'search-engines)
      '(("default" . "https://duckduckgo.com/?q=~a")
        ("!a" . "https://www.amazon.com/s?k=~a")
        ("!archwiki" . "https://wiki.archlinux.org/index.php?search=~a")
        ("!ddg" . "https://duckduckgo.com/?q=~a")
        ("!dpkg" . "https://packages.debian.org/search?keywords=~a")
        ("!g" . "https://www.google.com/search?hl=en&q=~a")
        ("!gh" . "https://github.com/search?utf8=%E2%9C%93&q=~a")
        ("!maps" . "https://maps.google.com/maps?q=~a")
        ("!s" . "https://startpage.com/do/metasearch.pl?query=~a")
        ("!w" . "https://en.wikipedia.org/wiki/Special:Search?search=~a&go=Go")
        ("!wt" . "https://en.wiktionary.org/wiki/Special:Search?search=~a&go=Go")
        ("!yt" . "https://www.youtube.com/results?search_query=~a")))

(setf (get-default 'blocker-mode 'hostlists)
      (list (make-instance 'hostlist
                           :url "https://raw.githubusercontent.com/anudeepND/blacklist/master/adservers.txt"
                           :path (xdg-data-home "hostlist-anudeepnd"))))

(define-parenscript %scroll-half-page-down ()
  (ps:chain window (scroll-by (ps:create top (/ (ps:@ window inner-height) 2)
                                         left 0
                                         behavior "smooth"))))

(define-parenscript %scroll-half-page-up ()
  (ps:chain window (scroll-by 0 (- (/ (ps:@ window inner-height)
                                      2)))))

(define-command scroll-half-page-down ()
  "Scroll down by half page height."
  (%scroll-half-page-down))

(define-command scroll-half-page-up ()
  "Scroll up by half page height."
  (%scroll-half-page-up))

;; missing: O T gi [[ ]] gu gU p gh gH u go gn

(define-key :scheme :vi-normal
  "["   nil
  "]"   nil
  "O"   nil
  "g b" nil
  "C-d" #'scroll-half-page-down
  "C-u" #'scroll-half-page-up
  "C-l" #'set-url-current-buffer
  "M-l" #'set-url-new-buffer
  "t"   #'set-url-new-buffer
  "d"   #'delete-current-buffer
  "D"   #'delete-buffer
  "b"   #'switch-buffer
  "g t" #'switch-buffer-next
  "g T" #'switch-buffer-previous)
(define-key :mode 'document-mode :scheme :vi-normal
  "y y" #'copy-url)

(when (uiop:getenv "WORKMAN")
  (define-key :mode 'document-mode :scheme :vi-normal
    "j"   nil
    "h"   nil
    "l"   nil
    "N"   nil
    "H"   nil
    "L"   nil
    "y y" nil
    "y u" nil
    "y t" nil
    "n"   #'scroll-down
    "e"   #'scroll-up
    "y"   #'scroll-left
    "o"   #'scroll-right
    "Y"   #'history-backwards
    "O"   #'history-forwards
    "k"   #'next-search-hint
    "K"   #'previous-search-hint
    "j j" #'copy-url
    "j u" #'copy-url
    "j t" #'copy-title)
  (define-key :scheme :vi-normal
    "o" nil
    "l" #'set-url-current-buffer)
  (define-key :mode 'help-mode :scheme :vi-normal
    "j" nil
    "k" nil
    "n" #'scroll-down
    "e" #'scroll-up))

;; (defun old-reddit-hook (url)
;;   (let* ((uri (quri:uri url)))
;;     (if (search "www.reddit.com" (quri:uri-host uri))
;;         (progn
;;           (setf (quri:uri-host uri) "old.reddit.com")
;;           (let ((new-url (quri:render-uri uri)))
;;             (log:info "Switching to old Reddit: ~a" new-url)
;;             new-url))
;;       url)))
;; (add-to-default-list #'old-reddit-hook 'buffer 'load-hook)
