(defconst emacs-start-time (current-time))

(prefer-coding-system 'utf-8-unix)

(eval-and-compile
  ;; Windows : change modifiers
  (when (string= system-type "windows-nt")
    (setq inhibit-compacting-font-caches t))

  ;; Mac : change modifiers
  (when (string= system-type "darwin")
    (setq mac-option-modifier 'none)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'super))

  (when (= emacs-major-version 24)
	(load (expand-file-name "init24.el" user-emacs-directory)))

  ;; Get the use-package command
  (add-to-list 'load-path (expand-file-name "use-package" user-emacs-directory))
  (require 'use-package)

  ;; Set t to install packages automatically
  (let ((elpa (expand-file-name "elpa" user-emacs-directory)))
	(if (not (file-exists-p elpa))
		(progn
		  (require 'package)
		  (package-initialize)
		  (setq use-package-always-ensure t))
	  (progn
		;; Put package install path into load path
		(mapc (lambda (path) (add-to-list 'load-path path))
			  (directory-files elpa t "^[^.].*-[0-9-]+")))))

  ;; 'y or n' au lieu de 'yes or no'
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Windmove modifier
  (windmove-default-keybindings 'meta)

  ;; pipe the full buffer if no region is selected
  (define-advice shell-command-on-region (:around (sh-reg start end &rest r) scr-on-buffer)
    "If no region is active, call the shell-command-on-region on the all buffer"
    (if (use-region-p)
        (apply sh-reg start end r)
      (apply sh-reg (point-min) (point-max) r)))

  ;; Pas d'écran d'accueil si on ouvre un fichier
  (when (> (length command-line-args) 1)
    (setq inhibit-splash-screen t))
  ) ;; eval-and-compile

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil)
 '(delete-selection-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(mouse-yank-at-point t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
	(yasnippet ido-vertical-mode web-mode ztree auto-complete macrostep cua evil powerline use-package smex magit)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red")))))

;; Variables pour retirer les avertissements sur certaines fonctions
(put 'narrow-to-region 'disabled nil)



(use-package org
  :mode ("\\.org\\'" . org-mode)

  :config
  ;; La face est safe si c'est une liste de liste d'association
  ;; de chaîne de caractères
  (put 'org-todo-keyword-faces 'safe-local-variable
       (lambda (val)
         (and (every 'stringp (mapcar 'car val))
              (every 'stringp (mapcar 'cdr val)))))
  (custom-set-variables '(org-support-shift-select t))
  ) ;; org


;; The use-package stuff
(use-package package
  :defer t
  :config
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)
  ) ;; package

(use-package cc-mode
  :defer t
  :init
  (setq c-basic-offset tab-width)
  ) ;; cc-mode

(use-package sh-mode
  :defer t
  :init
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode nil)))
  ) ;; sh-mode

(use-package elisp-mode
  :ensure nil
  :defer t
  :init
  (defun my/elisp-mode-hook ()
    (setq indent-tabs-mode nil))

  (add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hook)
  ) ;; elisp-mode

(use-package smerge-mode
  :defer t
  :bind (:map smerge-mode-map
              ("M-n" . smerge-next)
              ("M-p" . smerge-prev)
              ("M-m" . smerge-keep-mine)
              ("M-o" . smerge-keep-other)
              ("M-a" . smerge-keep-all))
  ) ;; smerge-mode

(use-package smex
  :after (ido-vertical-mode)
  :bind (("M-x" . smex))
  :config
  (require 'ido-vertical-mode)
  ) ;; smex

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  ) ;; paren

(use-package cua-mode
  :ensure nil

  ;; The cua-mode is set at the first call t C-RET.
  ;; So the first time it have to be called twice.
  :bind (("C-<return>" . cua-mode))

  :init
  (setq cua-enable-cua-keys nil)
  ) ;; cua

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))

  :config
  ;; Switching to ibuffer puts the cursor on the most recent buffer
  (define-advice ibuffer (:around (ibuf &rest r) ibuffer-point-to-most-recent)
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      (funcall ibuf)
      (ibuffer-jump-to-buffer recent-buffer-name)))

  ; Test de personalisation pour ibuffer
  ;; (load "~/.emacs.d/ibuffer-test.el")
  ) ;; ibuffer

(use-package magit
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log)
         :map magit-log-mode-map
         ("C-x C-f" . my/magit-find-file)
         ("C-x C-S-f" . magit-find-file))

  :config
  (defun my/magit-find-file ()
    "Equivalent of magit-find-file but does not ask for revision and use the default one."
    (interactive)
    (let* ((rev (magit-branch-or-commit-at-point))
           (file (magit-read-file-from-rev rev "Find file")))
      (switch-to-buffer (magit-find-file-noselect rev file))))
  ) ;; magit

(use-package powerline
  :if (string= system-type "windows-nt")

  :init
  (setq powerline-default-separator (quote wave))
  (setq powerline-display-mule-info t)
  (setq powerline-height nil)

  :config
  (powerline-center-theme)
  (set-face-attribute 'mode-line nil
		      :background "dark slate blue"
		      :foreground "gray")
  (set-face-attribute 'powerline-active1 nil
                      :background "navy")
  (set-face-attribute 'powerline-active2 nil
                      :background "slate blue")
  ) ;; powerline

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

(use-package yasnippet
  :defer t
  :commands yas-minor-mode

  :init
  (add-hook 'org-mode-hook #'yas-minor-mode)

  :config
  (yas-reload-all)
  ) ;; yasnippet

(use-package macrostep
  :defer t
  :commands macrostep-expand
  ) ;; macrostep

(use-package auto-complete
  ;; :bind (("M-/" . auto-complete))

  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-use-menu-map t)
  ;; (setq ac-auto-start nil)
  ;; (ac-set-trigger-key "M-/")
  ) ;; auto-complete

(use-package ztree
  :commands (ztree-dir ztree-diff)
  :config
  (setq ztree-dir-move-focus t)
  ) ;; ztree

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.php\\'"
  :config
  (setq web-mode-enable-auto-indentation nil)
  ) ;; web-mode

(use-package ido-vertical-mode
  :defer t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  ) ;; ido-vertical-mode

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
