;; -*- lexical-binding: t; -*-

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
