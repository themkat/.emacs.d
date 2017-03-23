(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)  ;; for newest version of org mode
(package-initialize)

(unless (file-exists-p (expand-file-name
                        (concat package-user-dir
                                "/archives/MELPA/")))
  (package-refresh-contents))

(dolist (package '(ac-geiser
		   ac-emoji
                   auto-complete
		   auctex
		   emojify
		   exwm
                   geiser
		   leuven-theme
		   markdown-mode
		   nyan-mode
		   org
                   paredit           
                   pretty-lambdada
		   slime))
  (unless (package-installed-p package)
    (package-install package)))


;; exwm (comment out this line if the computer isnt using exwm)
(load-file "~/.emacs.d/exwm.el")

;; shows battery status (useful when using EXWM)
(display-battery-mode 1)

;; make copy and paste use the same clipboard as emacs.
;; great for exwm
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)


;; Enable show-paren-mode (to visualize paranthesis) and make it possible to delete things we have marked
(show-paren-mode 1)
(delete-selection-mode 1)


;; use leuven for prettier org mode
(load-theme 'leuven t)

;; Removes the splash screen
(setq inhibit-splash-screen t)


;; Org mode
(setq org-startup-with-inline-images t)
(setq org-todo-keyword-faces '(("DONE" . "GREEN")))


;; Nyan cat (animated nyan cat instead of marker position in percentage)
(setq nyan-animate-nyancat t)
(setq nyan-wavy-trail t)
(nyan-mode)


;; emoji mode :)
(add-hook 'after-init-hook #'global-emojify-mode)


;; Latex
;; Preview of LaTeX formulae, tables, tikz drawings etc. 
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;;(set-default 'preview-scale-function 1.2)

;; Sets the zoom level of latex fragments
(defun update-org-latex-fragments ()  
  (with-current-buffer (current-buffer)
    (when (derived-mode-p 'LaTeX-mode 'TeX-mode 'latex-mode 'tex-mode)
      (set-default 'preview-scale-function text-scale-mode-amount)
      (preview-buffer))))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

;; make C-. the button for preview in latex mode
(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-.") 'preview-buffer))

;; let us use minted with the preview (minted fragments is not previewed :( )
(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))



;; Scheme specifics and autocomplete mode
(setq geiser-active-implementations '(racket))

;; autocomplete
(require 'auto-complete-config)
(require 'ac-geiser)
(ac-config-default)

;; ac-geiser recommended setup.
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; pretty-lambdada setup.
(add-to-list 'pretty-lambda-auto-modes 'geiser-repl-mode)
(pretty-lambda-for-modes)

;; Loop the pretty-lambda-auto-modes list.
(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))


;; making paredit work with delete-selection-mode
;; found on the excellent place called what the emacs d.
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)


;; Python jedi (bedre autocomplete)
;; Standard Jedi.el setting
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.
;; TODO: automate the above
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; autocomplete in org-mode
(add-hook 'org-mode-hook 'ac-emoji-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'org-mode))


;; Common Lisp
;; Sets the inferior lisp program
;; TODO: do other things that might be useful. (ac-slime etc.)
(setq inferior-lisp-program "/usr/bin/sbcl")



;;stuff autogenereatwd
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (try slime pdf-tools ac-emoji markdown-mode org nyan-mode auctex emojify leuven-theme jedi pretty-lambdada paredit exwm ac-geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
