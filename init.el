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
 '(backup-directory-alist (quote ((".*" . (format "%s/%s" user-emacs-directory "backups")))))
 '(blink-cursor-mode nil)
 '(delete-selection-mode t)
 '(dired-listing-switches "-alh")
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(echo-keystrokes 0.1)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(frame-resize-pixelwise t)
 '(mouse-yank-at-point t)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (dired-subtree wgrep ivy-rich counsel indent-tabs-nil highlight-indent-guides iedit dockerfile-mode cmake-mode rust-mode smart-tabs-mode flx-ido expand-region yaml-mode auctex yasnippet ido-vertical-mode web-mode ztree auto-complete macrostep cua evil powerline use-package smex magit)))
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-cursor nil)
 '(window-resize-pixelwise t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red")))))

;; local custom pour ne pas polluer l'init
(let ((local-custom (expand-file-name "local-custom.el" user-emacs-directory)))
  (if (not (file-exists-p local-custom))
      (write-region "" nil local-custom))
  (setq custom-file local-custom)
  (load custom-file))


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
  (add-to-list 'load-path (format "%s/%s" user-emacs-directory "el-get/el-get"))
  :config
  (add-to-list 'el-get-recipe-path (format "%s/%s" user-emacs-directory "el-get-user/recipes"))
  (el-get 'sync (append '(el-get) (mapcar #'el-get-source-name el-get-sources)))
  ) ; el-get


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  ) ;; wdired

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Faux paquets pour utliser certaines options

(use-package indent-tabs-nil
  ; Special false package to place indent-tabs-mode to nil on some program modes
  :no-require t
  :hook (sh-mode emacs-lisp-mode)
  :init
  (defun indent-tabs-nil ()
    (setq indent-tabs-mode nil))
  ) ;; indent-tabs-nil

(use-package non-window-emacs
  ; Options pour emacs en mode non-window (-nw)
  :no-require t
  :if (not (display-graphic-p))
  :bind (("C-v" . rectangle-mark-mode)
         ("C-c ;" . comment-line)) ;; C-; is not possible in a terminal

  :init
  ;; Certains shells utilise le prefix echap (^[ ou \e) pour le modificateur
  ;; meta. L'esc-map est correcte pour la plupart des combinaisons mais les
  ;; flèches ne fonctionnent pas toujours (comme avec urxvt...).
  (when (string-match "rxvt" (frame-parameter nil 'tty-type))
    (define-key key-translation-map (kbd "ESC <up>")    (kbd "M-<up>"))
    (define-key key-translation-map (kbd "ESC <down>")  (kbd "M-<down>"))
    (define-key key-translation-map (kbd "ESC <right>") (kbd "M-<right>"))
    (define-key key-translation-map (kbd "ESC <left>")  (kbd "M-<left>")))
  ) ;; non-window-emacs

(use-package toggle-visual-element
  ; Vol éhonté à Protesilaos :) -> https://gitlab.com/protesilaos/dotemacs/blob/master/emacs-init.org
  :no-require t
  :init
  (defmacro def-toggle-mode (mode-sym &optional docstring)
    "Create toggle function for the mode"
    (let* ((sym-name (symbol-name mode-sym))
           (f-name (intern (concat "toggle/" sym-name)))
           (callee (intern (concat sym-name "-mode"))))
      `(defun ,f-name ()
         ,(format "%s\nCall `%s' as the toggle" docstring (symbol-name callee))
         (interactive)
         (if (bound-and-true-p ,callee)
             (,callee -1)
           (,callee))
         )))

  (def-toggle-mode display-line-numbers
    "Toggles the display of line numbers. Applies to all buffers.")
  (def-toggle-mode whitespace
    "Toggles the display of indentation and space characters.")
  (def-toggle-mode hl-line
    "Toggles the highlight of the current line.")

  :bind (("C-c l n" . toggle/display-line-numbers)
         ("C-c l w" . toggle-truncate-lines)
         ("C-c l h" . toggle/hl-line)
         ("C-c i" . toggle/whitespace))
  ) ;; toggle-visual-element

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autre paquets

(use-package cl-lib
  ;; Les fonctions de cl seront chargées à la demande
  :commands cl-every
  )

(use-package org
  :mode ("\\.org\\'" . org-mode)

  :config
  ;; La face est safe si c'est une liste de liste d'association
  ;; de chaîne de caractères
  (put 'org-todo-keyword-faces 'safe-local-variable
       (lambda (val)
         (and (cl-every 'stringp (mapcar 'car val))
              (cl-every 'stringp (mapcar 'cdr val)))))
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
  :disabled
  :defer t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  ) ;; ido-vertical-mode

(use-package flx-ido
  :disabled
  :defer t
  :config
  (flx-ido-mode 1)
  ; enable flx highlight
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  ) ;; flx-ido

(use-package smex
  :disabled
  :bind (("M-x" . smex))
  :config
  (require 'ido-vertical-mode)
  (require 'flx-ido)
  ) ;; smex

(use-package amx
  ;; Package to sort ivy M-x entries
  :after counsel
  ) ;; amx

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . ivy-switch-buffer)
         ("C-x g f" . counsel-git))
  :config
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
  ) ;; counsel

(use-package ivy-rich
  :after ivy counsel
  :config
  (require 'counsel) ; doit requérir counsel pour activer ivy-rich partout
  (ivy-rich-mode 1)
  ) ;; ivy-rich

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  ) ;; paren

(use-package ibuffer
  :if (version<= "24.4" emacs-version)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         (":" . ibuffer-switch-format))

  :custom
  ;; Increase the space for file name
  (ibuffer-formats '((mark modified read-only " "
                           (name 24 24 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " " filename-and-process)
                     (mark modified read-only " "
                           (name 24 -1)
                           " " filename)))
  (ibuffer-eliding-string "…")

  :config
  ;; Switching to ibuffer puts the cursor on the most recent buffer
  (define-advice ibuffer (:around (ibuf &rest r) ibuffer-point-to-most-recent)
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      (funcall ibuf)
      (ibuffer-jump-to-buffer recent-buffer-name)))

  ; Test de personalisation pour ibuffer
  ;; (load (format "%s/%s" user-emacs-directory "ibuffer-test.el"))
  ) ;; ibuffer

(use-package magit
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log)
         ("C-x g b" . magit-blame)
         ("C-x g g" . magit-blame) ;; vc compatible shortcut
         ("C-x g r" . vc-git-grep)
         :map magit-log-mode-map
         ("C-x C-f" . my/magit-find-file)
         ("C-x C-S-f" . magit-find-file)
         :map magit-file-mode-map
         ("C-x g" . nil))

  :custom
  (magit-diff-refine-hunk t "Highlight word change in diff")
  (transient-display-buffer-action '(display-buffer-below-selected (side . bottom)) "Popup below magit buffer")
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))

  :config
  (defun my/magit-find-file ()
    "Equivalent of magit-find-file but does not ask for revision and use the default one."
    (interactive)
    (let* ((rev (magit-branch-or-commit-at-point))
           (file (magit-read-file-from-rev rev "Find file")))
      (switch-to-buffer (magit-find-file-noselect rev file))))
  ) ;; magit

(use-package evil
  :commands evil-mode
  ) ;; evil

(use-package yasnippet
  :commands yas-minor-mode
  :hook (org-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ) ;; yasnippet

(use-package macrostep
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
  :mode "\\.dockerfile\\'"
  ) ;; dockerfile-mode

(use-package rust-mode
  :mode "\\.rs\\'"
  ) ;; rust-mode

(use-package iedit
  :bind ("C-;" . iedit-mode)
  ) ;; help-mode

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  ) ;; highlight-indent-guides

(use-package wgrep
  :after grep
  :custom
  (wgrep-auto-save-buffer t)
  ) ;; wgrep

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle))
  ) ;; dired-subtree

(use-package smart-mode-line
  ;; :disabled
  :custom
  (sml/theme 'dark)
  (sml/modified-char "*")
  (sml/vc-mode-show-backend t)

  :custom-face
  (sml/git ((t (:inherit sml/read-only))))

  :config
  (sml/setup)
) ; smart-mode-line

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
