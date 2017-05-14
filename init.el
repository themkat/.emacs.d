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
		   cider
		   emojify
		   exwm
		   flyspell-popup
		   focus
                   geiser
		   helm
		   helm-projectile
		   leuven-theme
		   magit
		   markdown-mode
		   multiple-cursors
		   nyan-mode
		   org
                   paredit
		   projectile
                   pretty-lambdada
		   slime
		   try))
  (unless (package-installed-p package)
    (package-install package)))


;; helm specifics (may have to be moved)
;; TODO: check if this interferes with the exwm ido stuff
(require 'helm)
(require 'helm-config)
(require 'projectile)
(helm-mode 1)
(projectile-global-mode 1)
(helm-projectile-on)
(helm-adaptive-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini) ;; get helms awesome buffer list instead of the standard stuff
(setq helm-M-x-fuzzy-match t
      projectile-completion-system 'helm
      helm-split-window-in-side-p t)
;; hide uninteresting buffers
(add-to-list 'helm-boring-buffer-regexp-list (rx "*magit-"))
(add-to-list 'helm-boring-buffer-regexp-list (rx "*helm"))

;; use y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)


;; kill all those horrible old buffers from magit, helm etc.
;; they really clutter up the buffer list, which is tiresome. especially when using exwm.
;;(require 'midnight)
;;(setq midnight-period 3600)
;; TODO: fix this so C-x <arrowkey> is usable again



;; exwm (comment out this line if the computer isnt using exwm)
(load-file "~/.emacs.d/exwm.el")


;; disable the C-z sleep/suspend key
;; rarely use emacs in terminal mode anymore and that is the only place it can be useful
;; see http://stackoverflow.com/questions/28202546/hitting-ctrl-z-in-emacs-freezes-everything
;;  for a way to have both if I ever want that again.
(global-unset-key (kbd "C-z"))


;; shows battery status (useful when using EXWM)
(display-battery-mode 1)

;; make copy and paste use the same clipboard as emacs.
;; great for exwm
(setq x-select-enable-primary t
      x-select-enable-clipboard t)

;; sets monday to be the first day of the week in calendar
(setq calendar-week-start-day 1)


;; Enable show-paren-mode (to visualize paranthesis) and make it possible to delete things we have marked
(show-paren-mode 1)
(delete-selection-mode 1)


;; use leuven for prettier org mode
(load-theme 'leuven t)

;; Removes the splash screen
(setq inhibit-splash-screen t)


;; Nyan cat (animated nyan cat instead of marker position in percentage)
;; (setq nyan-animate-nyancat t
;;       nyan-wavy-trail t)
;; (nyan-mode)
;; got tired of Nyan cat :/ 


;; multiple cursors
(global-set-key (kbd "M-s-e") 'mc/edit-lines)
(global-set-key (kbd "M-s-n") 'mc/insert-numbers)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

;; regex date pattern (norwegian)
;; useful for stuff below (and a small project I'm working on)
(define-key minibuffer-local-map (kbd "M-s-d") "[0-9]\\\{2\\\}\\\/[0-9]\\\{2\\\}\\\/[0-9]\\\{4\\\}")

;; go through all marked text and reorder the dates to yyyy-mm-dd
;; no error checking, so I use it only with multiple cursors. lazy..
;; it could probably be done better
(defun reorder-dates-helper ()
  (let* ((marked-text (buffer-substring (mark) (point)))
	 (day (substring marked-text 0 2))
	 (month (substring marked-text 3 5))
	 (year (substring marked-text 6 10)))
    (delete-region (mark) (point))
    (insert (concat year "-" month "-" day))))

;; ugly af, but it seems to be the only iteration method that
;; works for mc/cycle-forward
(defun reorder-dates ()
  (interactive)
  (let ((count (mc/num-cursors)))
    (while (> count 0)
      (reorder-dates-helper)
      (mc/cycle-forward)
      (setq count (- count 1)))
    (multiple-cursors-mode 0)
    (deactivate-mark)))



;; emoji mode :)  
(add-hook 'after-init-hook #'global-emojify-mode)


;; FlySpell (spell checking)
(dolist (flyspellmodes '(text-mode-hook
			 org-mode-hook
			 latex-mode-hook))
  (add-hook flyspellmodes 'turn-on-flyspell))

;; comments and strings in code
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; sets american english as defult 
(setq ispell-dictionary "american")

;; let us cycle american english (best written english) and norwegian 
;; TODO: get the spelling checks to be updated when changing language (and not just the new words)
(defun change-dictionary ()
  (interactive)
  (ispell-change-dictionary (if (string-equal ispell-current-dictionary "american")
				"norsk"
			        "american")))

;; use flyspell popup to autocomplete words (overwrites standard way)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") #'flyspell-popup-correct))

;; makes my latex hotkey work. (I only use C-, for correcting anyway)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))


;; Latex 
;; Preview of LaTeX formulae, tables, tikz drawings etc. 
(setq TeX-auto-save t)
(setq TeX-parse-self t)

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


;; Magit (git for emacs!!! :D )
(global-set-key (kbd "C-x g") 'magit-status)


;; autocomplete mode
;; TODO: try company mode and check if its better
(require 'auto-complete-config)
(ac-flyspell-workaround) ;; flyspell works terrible with autocomplete, this compensates it

;; Scheme specifics and autocomplete mode
(setq geiser-active-implementations '(racket))

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



;; Org mode
(setq org-startup-with-inline-images t
      org-todo-keyword-faces '(("DONE" . "GREEN")))

;; autocomplete in org-mode
(add-hook 'org-mode-hook 'ac-emoji-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'org-mode))


;; Let us have autocomplete for emojis in markdown-mode
;; TODO: get this to work. See if it maybe work better with company mode
;;(add-hook 'markdown-mode-hook 'ac-emoji-setup)


;; Common Lisp
;; Sets the inferior lisp program
;; TODO: do other things that might be useful. (ac-slime etc.)
(setq inferior-lisp-program "/usr/bin/sbcl")



;;stuff auto-generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes
   (quote
    ("0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" default)))
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (focus helm-projectile projectile helm cider magit try slime pdf-tools ac-emoji markdown-mode org nyan-mode auctex emojify leuven-theme jedi pretty-lambdada paredit exwm ac-geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )