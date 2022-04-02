;; -*- lexical-binding: t; -*-

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
  (setf ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-plus)
          (t . ivy--regex-plus)))
  ) ;; counsel

(use-package ivy-rich
  :after (ivy counsel)
  :config
  ;; (or (featurep 'ivy) (require 'ivy)) ; doit requérir counsel pour activer ivy-rich partout
  ;; (or (featurep 'counsel) (require 'counsel)) ; doit requérir counsel pour activer ivy-rich partout
  (ivy-rich-mode 1)
  ) ;; ivy-rich
