(defconst emacs-start-time (current-time))

;; Necessary
;(package-initialize)

(eval-and-compile
  ;; Mac : change modifiers
  (when (string= system-type "darwin")
    (setq mac-option-modifier 'none)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'super))

  ;; Put package install path into load path
  (mapc (lambda (path)
	  (add-to-list 'load-path path))
	(directory-files
	 (expand-file-name "elpa" user-emacs-directory) t "^[^.].*-[0-9-]+"))

  (require 'use-package)
  (when nil
    ;; Set t to install packages automatically
    (require 'package)
    (package-initialize)
    (setq use-package-always-ensure t))
  )

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; The use-package stuff
(use-package package
  :defer t
  :config
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(use-package smex
  :bind (("M-x" . smex))
  ) ;; smex

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))

  :config
  ;; Switching to ibuffer puts the cursor on the most recent buffer
  (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
	     "Open ibuffer with cursor pointed to most recent buffer name"
	     (let ((recent-buffer-name (buffer-name)))
	       ad-do-it
	       (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer)

  ) ;; ibuffer

(use-package magit
  :bind (("C-x g s" . magit-status))
  ) ;; magit

;; Gives the loading time
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
