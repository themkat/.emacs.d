;; Tangle the source (get the Emacs Lisp source code pieces) using org mode, and then run it :)
;; (this file is replaced on first run)
(require 'org)
(find-file (concat user-emacs-directory "init.org"))
(org-babel-tangle)
(let ((init-file (concat user-emacs-directory "init.el")))
  (load-file init-file)
  ;; byte compile to make it faster at startup
  (byte-compile-file init-file))
