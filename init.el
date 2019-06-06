;;; init.el --- -*- lexical-binding: t; coding:utf-8; fill-column: 120 -*-

(defconst emacs-start-time (current-time))

(prefer-coding-system 'utf-8-unix)

(pcase system-type
  ;; Windows spécifique
  ("windows-nt"
   (setq inhibit-compacting-font-caches t))
  ;; Mac spécifique
  ("darwin"
   (setq mac-option-modifier 'none)
   (setq mac-command-modifier 'meta)
   (setq mac-right-command-modifier 'super)))


;; Get the use-package command
(add-to-list 'load-path (expand-file-name "use-package" user-emacs-directory))
(require 'use-package)

;; Les paquets s'installe automatiquement si le dossier elpa n'existe
;; pas. Sinon le plugin 'package' n'est pas chargé pour accélérer
;; le démarrage.
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

;; Config spécifiques par machine
(load (expand-file-name "local-init.el" user-emacs-directory))

(when (= emacs-major-version 24)
  (load (expand-file-name "emacs24-init.el" user-emacs-directory)))

;; 'y or n' au lieu de 'yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Windmove modifier
(windmove-default-keybindings 'meta)
;; Certains shells utilise le prefix echap (^[ ou \e) pour le modificateur
;; meta. L'esc-map est correcte pour la plupart des combinaisons mais les
;; flèches ne fonctionnent pas toujours (comme avec urxvt...).
(unless (display-graphic-p)
  (define-key key-translation-map (kbd "ESC <up>")    (kbd "M-<up>"))
  (define-key key-translation-map (kbd "ESC <down>")  (kbd "M-<down>"))
  (define-key key-translation-map (kbd "ESC <right>") (kbd "M-<right>"))
  (define-key key-translation-map (kbd "ESC <left>")  (kbd "M-<left>")))

;; pipe the full buffer if no region is selected
(define-advice shell-command-on-region (:around (sh-reg start end &rest r) scr-on-buffer)
  "If no region is active, call the shell-command-on-region on the all buffer"
  (if (use-region-p)
      (apply sh-reg start end r)
    (apply sh-reg (point-min) (point-max) r)))

;; Pas d'écran d'accueil si on ouvre un fichier
(when (> (length command-line-args) 1)
  (setq inhibit-splash-screen t))

;; Retire l'indentation automatique (electric indent) sur la touche entrée
(setq electric-indent-chars (remq ?\n electric-indent-chars))

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
    (highlight-indent-guides iedit dockerfile-mode cmake-mode rust-mode smart-tabs-mode flx-ido expand-region yaml-mode auctex yasnippet ido-vertical-mode web-mode ztree auto-complete macrostep cua evil powerline use-package smex magit)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red")))))

;; Variables pour retirer les avertissements sur certaines fonctions
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The use-package stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package package
  :defer t
  :config
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)
  ) ;; package

(use-package el-get
  ; el-get installe les paquets spécifiques mais comme pour package
  ; on ne le charge qu'au premier démarrage.
  :if (featurep 'package)
  :init
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  :config
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
  (el-get 'sync (append '(el-get) (mapcar #'el-get-source-name el-get-sources)))
  ) ; el-get


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Paquets internes de emacs (pas de 'ensure' pour ne pas les modifier)
(use-package sh-mode
  :ensure nil
  :defer t
  :init
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ) ;; sh-mode

(use-package cua-mode
  :ensure nil

  ;; The cua-mode is set at the first call to C-RET.
  ;; So the first time it have to be called twice.
  :bind (("C-<return>" . cua-mode))

  :init
  (setq cua-enable-cua-keys nil)
  ) ;; cua

(use-package help-mode
  :ensure nil
  :defer t
  :bind (:map help-mode-map
              ("f" . help-go-forward)
              ("b" . help-go-back))
  ) ;; help-mode

(use-package conf-mode
  :ensure nil
  :defer t
  :bind (:map conf-mode-map
              ("C-c t" . my/conf-toogle-bool))
  :config
  (defun my/conf-toogle-bool ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "=\\s-*\\(true\\|false\\|0\\|1\\)" (line-end-position) t)
      ;; (message (match-string 1))
      (replace-match (pcase (downcase (match-string 1))
                       ("true" "false")
                       ("false" "true")
                       ("0" "1")
                       ("1" "0"))
                     nil nil nil 1))))
  ) ;; conf-mode

(use-package prettify-symbols-mode
  :ensure nil
  :hook ((emacs-lisp-mode python-mode) . my/prettify-symbol-hook)
  :init
  (defun my/prettify-symbol-hook ()
    (progn
      (setq prettify-symbols-alist
            '(("lambda" . ?λ)
              (    ">=" . ?≥)
              (    "<=" . ?≤)))
      (prettify-symbols-mode)))
  ) ;; prettify-symbols-mode

(use-package indent-tabs-nil
  ; Special false package to place indent-tabs-mode
  ; to nil on some program modes
  :no-require t
  :hook (sh-mode emacs-lisp-mode)
  :init
  (defun indent-tabs-nil ()
    (setq indent-tabs-mode nil))
  ) ;; indent-tabs-nil

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autre paquets

(use-package cl
  ;; Les fonctions de cl seront chargées à la demande
  :commands every
  )

(use-package org
  :mode ("\\.org\\'" . org-mode)

  :config
  ;; La face est safe si c'est une liste de liste d'association
  ;; de chaîne de caractères
  (put 'org-todo-keyword-faces 'safe-local-variable
       (lambda (val)
         (and (every 'stringp (mapcar 'car val))
              (every 'stringp (mapcar 'cdr val)))))
  (setq org-support-shift-select t)
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (awk . t)
                                 (python . t)
                                 (shell . t)))
  (when (string= system-type "windows-nt")
    ;; Lanceur python spécifique pour windows
    (setq org-babel-python-command "py -3"))
  ) ;; org

(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset tab-width)
  (setcdr (assoc 'other c-default-style) "linux")
  (c-set-offset 'innamespace 0)
  ) ;; cc-mode

(use-package smerge-mode
  :defer t
  :bind (:map smerge-mode-map
              ("M-n" . smerge-next)
              ("M-p" . smerge-prev)
              ("M-m" . smerge-keep-mine)
              ("M-o" . smerge-keep-other)
              ("M-a" . smerge-keep-all))
  ) ;; smerge-mode

(use-package ido-vertical-mode
  :defer t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  ) ;; ido-vertical-mode

(use-package flx-ido
  :defer t
  :config
  (flx-ido-mode 1)
  ; enable flx highlight
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  ) ;; flx-ido

(use-package smex
  :bind (("M-x" . smex))
  :config
  (require 'ido-vertical-mode)
  (require 'flx-ido)
  ) ;; smex

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  ) ;; paren

(use-package ibuffer
  :if (version<= "24.4" emacs-version)
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

  (setq magit-diff-refine-hunk t)
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
  :hook (org-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ) ;; yasnippet

(use-package macrostep
  :defer t
  :commands macrostep-expand
  ) ;; macrostep

(use-package auto-complete
  :bind (:map ac-mode-map
              ("M-/" . auto-complete))
  ;; :bind (("M-/" . auto-complete))

  :init
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)

  :config
  (setq ac-use-menu-map t)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "M-/")
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

(use-package auctex
  :defer t
  :init
  ;; Depuis le manuel de auctex.
  ;; preview-latex n'est pas présent (peut-être été renommé en preview ?)
  (load "auctex.el" nil t t)
  ; (load "preview-latex.el" nil t t)
  ) ;; auctex

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :mode "\\.yml\\'"
  ) ;; yaml-mode

(use-package expand-region
  :bind (("C-=" . er/expand-region))
  ) ;; expand-region

(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++)
  )

(use-package cmake-mode
  :mode "CMakeLists.txt"
  ) ;; cmake-mode

(use-package dockerfile-mode
  :mode "/Dockerfile"
  ) ;; dockerfile-mode

(use-package rust-mode
  :mode "\\.rs\\'"
  ) ;; rust-mode

(use-package iedit
  :bind ("C-;" . iedit-mode)
  ) ;; help-mode

(use-package highlight-indent-guides
  :defer t
  :commands highlight-indent-guides-mode
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  ) ;; highlight-indent-guides

;; End of init : Gives the loading time
(when (display-graphic-p)
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
