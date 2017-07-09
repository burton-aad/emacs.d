(defconst emacs-start-time (current-time))

(eval-and-compile
  ;; Mac : change modifiers
  (when (string= system-type "darwin")
    (setq mac-option-modifier 'none)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'super))

  ;; Get the use-package command
  (add-to-list 'load-path (expand-file-name "use-package" user-emacs-directory))
  (require 'use-package)

  ;; Set t to install packages automatically
  (if nil
      (progn
        (require 'package)
        (package-initialize)
        (setq use-package-always-ensure t))
    (progn
      ;; Put package install path into load path
      (mapc (lambda (path)
              (add-to-list 'load-path path))
            (directory-files
             (expand-file-name "elpa" user-emacs-directory) t "^[^.].*-[0-9-]+"))))

  ;; 'y or n' au lieu de 'yes or no'
  (defalias 'yes-or-no-p 'y-or-n-p)
  )

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(package-enable-at-startup nil)
 '(package-selected-packages (quote (evil powerline use-package smex magit)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red")))))


;; The use-package stuff
(use-package package
  :defer t
  :config
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)
  ) ;; package

(use-package smex
  :bind (("M-x" . smex))
  ) ;; smex

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1))

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
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log))
  ) ;; magit

(use-package evil
  :defer t
  :init
  (defun evil-mode ()
    "This is only a dummy function to make it available while not autoloaded.

In the evil package the evil-mode function is not autoload so it is not
available through the use-package :commands keyword. So I made this
function to load evil and call the evil-mode so the evil package loading
can be defer.

Once called, this function will be replaced with the one from the evil package."
    (interactive)
    (require 'evil)
    (evil-mode))
  ) ;; evil



;; End of init : Gives the loading time
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
