;;; -*- lexical-binding: t -*-
(require 'org)
(when (not (file-exists-p "~/.emacs.d/emacs.el"))
  (org-babel-tangle-file "~/.emacs.d/emacs.org"))
(load-file "~/.emacs.d/emacs.el")
