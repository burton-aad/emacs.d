(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "init time %s" (emacs-init-time))))
