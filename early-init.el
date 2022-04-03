(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)
            (message "init time %s" (emacs-init-time))))
