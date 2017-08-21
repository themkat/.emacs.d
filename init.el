(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)  ;; for newest version of org mode
(package-initialize)

(unless (file-exists-p (expand-file-name
                        (concat package-user-dir
                                "/archives/MELPA/")))
  (package-refresh-contents))

(dolist (package '(all-the-icons
		   auctex
		   cider
		   company
		   company-emacs-eclim
		   dashboard
		   eclim
		   elm-mode
		   emojify
		   exwm
		   flycheck
		   focus
                   geiser
		   git-gutter-fringe
		   grandshell-theme
		   helm
		   helm-flycheck
		   helm-flyspell
		   helm-google
		   helm-projectile
		   helm-swoop
		   hlinum
		   java-snippets
		   js2-mode
		   leuven-theme
		   magit
		   markdown-mode
		   multiple-cursors
		   ng2-mode
		   neotree
		   nyan-mode
		   olivetti
		   org
		   org-bullets
                   paredit
		   page-break-lines
		   projectile
                   pretty-lambdada
		   rainbow-mode
		   slime
		   tide
		   try
		   undo-tree
		   yasnippet
		   web-mode))
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
(add-to-list 'helm-boring-buffer-regexp-list (rx "*magit"))
(add-to-list 'helm-boring-buffer-regexp-list (rx "*helm"))

;; helm-swoop for better searching
(require 'helm-swoop)
(setq helm-swoop-speed-or-color t)

;; use y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)


;; add rainbow colors to all colors (hexadecimal codes, red/blue/green, html-codes etc.)
(add-hook 'prog-mode-hook 'rainbow-mode)


;; setup a custom fancy startup screen
(require 'dashboard)
(dashboard-setup-startup-hook)

;; TODO: add org-agenda when you start to use it more
(setq dashboard-banner-logo-title "Welcome! Make some kewl stuff today!"
      dashboard-startup-banner 'logo
      dashboard-items '((projects . 5)
			(recents . 5)))
;; TODO: look into maybe adding some EXWM specific widgets or other widgets to dashboard 


;; simple theme switcher form the best org mode (and light) theme (leuven)
;;  to the best dark theme (grandshell)
(require 'cl) ;; for lexical-let
(lexical-let ((theme-cycle '#1=(leuven grandshell . #1#)))
  (defun next-theme ()
    (interactive)
    (disable-theme (car theme-cycle))
    (setq theme-cycle (cdr theme-cycle))
    (load-theme (car theme-cycle) t)
    (exwm-systemtray--refresh))) ;; for refreshing the system tray in exwm.
;; TODO: find a way of checking if exwm is used and test that in next-theme. now I have to comment out the
;;         last line on machines which are not using exwm

;; use undo tree for better undo/redo operations
;; TODO: get it to work better with linum mode if you encounter problems
(global-undo-tree-mode)


;; exwm (comment out this line if the computer isnt using exwm)
(load-file "~/.emacs.d/exwm.el")


;; disable the C-z sleep/suspend key
;; rarely use emacs in terminal mode anymore and that is the only place it can be useful
;; see http://stackoverflow.com/questions/28202546/hitting-ctrl-z-in-emacs-freezes-everything
;;  for a way to have both if I ever want that again.
(global-unset-key (kbd "C-z"))

;; disable the C-x C-b key, because I use helm instead
(global-unset-key (kbd "C-x C-b"))

;; shows battery status (useful when using EXWM)
(display-battery-mode 1)

;; make copy and paste use the same clipboard as emacs.
;; great for exwm
(setq x-select-enable-primary t
      x-select-enable-clipboard t)

;; sets monday to be the first day of the week in calendar
(setq calendar-week-start-day 1)


;; save emacs backups in a different directory
;; (some build-systems build automatically all files with a prefix, and .#something.someending breakes that)
(setq backup-directory-alist '(("." . "~/.emacsbackups")))


;; neotree. maybe it will be good for projects in the future. idk. it looks kewl
(require 'all-the-icons)
(setq neo-theme 'icons)
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)

;; Enable show-paren-mode (to visualize paranthesis) and make it possible to delete things we have marked
(show-paren-mode 1)
(delete-selection-mode 1)


;; use leuven for prettier org mode
(load-theme 'leuven t)

;; Removes the splash screen
;(setq inhibit-splash-screen t)


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

;; use helm-flyspell to correct words
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-,") #'helm-flyspell-correct))

;; makes my latex hotkey work. (I only use C-, for correcting anyway)
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))


;; FlyCheck (syntax checking)
(require 'flycheck)
(setq flycheck-indication-mode nil)
(setq flycheck-highlighting-mode 'lines)

;; I get bugs with flycheck globally (in elisp probably because I'm lazy with eval-after-load-stuff). Use it in selected modes instead
(add-hook 'python-mode-hook 'flycheck-mode)



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

;; git gutter fringe for showing changes in files
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)
(setq-default left-fringe-width 20)
;; TODO: try to fix the shit look of leuven-theme + git-gutter-fringe

;; update git-gutter-fringe icons when something gets updated in magit (staging, pushing etc.)
(add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)


;; add line numbers to programming modes
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")
;;(set-face-attribute 'linum nil :height 200)
;; TODO: find a way of making the scaling work ok

;; highlight current line-number
(require 'hlinum)
(hlinum-activate)


;; company mode for auto-completion
;; Decided to replace auto-complete based upon better support and (much) faster completion with company)
;; Also less configs needing to be done :D
(add-hook 'after-init-hook 'global-company-mode)

;; set the completion to begin at once
(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 1)

;; trigger company to see a list of choices even when nothing is typed. maybe it quit because we clicked something. or maybe we dont know what to type yet :P
;; CTRL-ENTER. Because C-RET does not work. 
(global-set-key [(control return)] 'company-complete)

;; Scheme specifics and autocomplete mode
(setq geiser-active-implementations '(racket))


;; Common Lisp
;; Sets the inferior lisp program
;; TODO: do other things that might be useful. (ac-slime etc.)
(setq inferior-lisp-program "/usr/bin/sbcl")

;; pretty-lambdada setup.
(add-to-list 'pretty-lambda-auto-modes 'geiser-repl-mode)
(pretty-lambda-for-modes)

;; Loop the pretty-lambda-auto-modes list.
(dolist (mode (cons 'clojure-mode pretty-lambda-auto-modes))
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


;; Java (may need to be branched out to own file eventually)
;; (untested)
(require 'eclim)
(setq eclimd-autostart t)
(add-hook 'java-mode-hook '(lambda () (eclim-mode t)))

;; setting directories (may have to be tweaked on other machines)


;; displaying error messages in the minibuffer
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1) ;; should this be smaller? will probably lead to small delays..
(help-at-pt-set-timer)

;; autocomplete with company
;; will it intellisense? DaH mu'tlheghvam vIqelnIS
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

;; setup yasnippet for java
;; TODO: maybe remove the first function earlier and use yasnippet for more modes
;;       seems good. still in the testing phase
(yas-reload-all)
(add-hook 'java-mode-hook #'yas-minor-mode)

;; useful java/eclim hotkeys
(add-hook 'java-mode-hook '(lambda ()
			     ;; open the selection of possible classes to import on ALT-ENTER. One of the few good things from bigger IDEs
;; this could also be a good fullscreen hotkey maybe?
			     (define-key java-mode-map (kbd "M-RET") #'eclim-java-import-organize)))

;; Org mode
(setq org-startup-with-inline-images t
      org-todo-keyword-faces '(("DONE" . "GREEN"))
      org-hide-emphasis-markers t)

;; make org mode easier to read with indentation
(add-hook 'org-mode-hook 'org-indent-mode)

;; add bullets to prettify the org mode stars
;; TODO: experiment with possible bullets to use
;;       set org-bullets-bullet-list to a list of strings with bullets ordered by "depth"
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; add olivetti to org-mode for better writing
(add-hook 'org-mode-hook 'olivetti-mode)
;;(olivetti-set-width 800)
;; TODO: find a good width for olivetti


;; web mode for necessary file formats
;; (may need to be extended when I try to use more)
;; Adds correct syntax highlighting to "mixed files" (javascript in a html document for instance)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; javascript
;; js2 mode with improved syntax highlighting and flyckecking
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(setq js2-include-node-externs t)

;; TODO: find a way of getting javascript completion with node_modules folder


;; typescript
;; set indent level to be the same as files generated by the project tools
(setq typescript-indent-level 2)

;; use standard settings from tide (typescript interactive development environment) github page
;; TODO: experiment with the settings
(add-hook 'typescript-mode-hook '(lambda ()
				   (tide-setup)
				   (flycheck-mode 1)
				   (setq flycheck-check-syntax-automatically '(save mode-enabled))
				   (eldoc-mode 1)
				   (tide-hl-identifier-mode 1)))

;; add typescripts tslint to flycheck-mode
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

;; TODO: get project files with angular to be checked correctly


;; elm
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-elm))
(setq elm-format-on-save t)


;;stuff auto-generated

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
 '(eclim-eclipse-dirs (quote ("~/eclipse")))
 '(eclim-executable "~/eclipse/eclim")
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (java-snippets js2-mode company-emacs-eclim eclim elm-mode tide ng2-mode helm-flycheck flycheck company-emoji company-mode web-mode undo-tree org-bullets rainbow-mode focus helm-projectile projectile helm cider magit try slime pdf-tools ac-emoji markdown-mode org nyan-mode auctex emojify leuven-theme jedi pretty-lambdada paredit exwm ac-geiser))))
