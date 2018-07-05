;; Basically just standard EXWM setup with modifications.

;; let you control standard os keys in exwm using desktop-environment package
(require 'desktop-environment)
(desktop-environment-mode)

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Shrink fringes to 1 pixel
(fringe-mode 1)

;; You may want Emacs to show you the time
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t display-time-24hr-format t)
(display-time-mode t)

;; You are strongly encouraged to enable `ido-mode' (or something similar) to
;; alter to default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however)
;; You may also want to call `exwm-enable-ido-workaround' later (see below)
(ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section)
(server-start)

;;;; Below are configurations for EXWM

;; Load EXWM
(require 'exwm)

;; Fix problems with Ido
(require 'exwm-config)
(exwm-config-ido)

;; Set the initial number of workspaces.
(setq exwm-workspace-number 2)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; `exwm-input-set-key' allows you to set a global key binding (available in
;; any case). Following are a few examples.
;; + We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; + Bind a key to switch workspace interactively
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; + Bind "s-0" to "s-9" to switch to the corresponding workspace.
(dotimes (i 4)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
;; + Application launcher ('M-&' also works if the output buffer does not
;;   bother you). Note that there is no need for processes to be created by
;;   Emacs.
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
;; + 'slock' is a simple X display locker provided by suckless tools.
(exwm-input-set-key (kbd "s-<f2>")
                    (lambda () (interactive) (start-process "" nil "slock")))

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-d] . delete)
   ([?\C-k] . (S-end delete))))

;; You can hide the mode-line of floating X windows by uncommenting the
;; following lines
;(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
;(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line
;;(setq exwm-workspace-minibuffer-position 'bottom)

;; system tray for Dropbox, Skype volume control, wireless manager etc.
(require 'exwm-systemtray)
(exwm-systemtray-enable)


;; turn on multimonitor support
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "DP-1-1"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
	    (start-process-shell-command
	     "xrandr" nil "xrandr --output DP-1-1 --right-of eDP-1-1 --auto")))
(exwm-randr-enable)


;; Do not forget to enable EXWM. It will start by itself when things are ready.
(exwm-enable)


;; Shortcut for starting Skype. To be able to use M-x skype is an almost surreal experience :P 
(defun skype ()
  (interactive)
  (start-process-shell-command "LD_PRELOAD=/usr/lib/i386-linux-gnu/mesa/libGL.so.1 skype" nil "LD_PRELOAD=/usr/lib/i386-linux-gnu/mesa/libGL.so.1 skype"))


;; Initial buffer on my EXWM setup: open a todo list
;; TODO: get this to work. now it makes system tray icons take up 20 times more space...
;; (setq initial-buffer-choice "~/TODO.org")
;;
;; Alternative solution that produces the same result (and also zooms the text to a comfortable level)
;; (let ((buf (find-file "~/TODO.org")))
;;   (with-current-buffer "TODO.org"
;;     (text-scale-set 5)))
