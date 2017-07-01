(defconst emacs-start-time (current-time))

;; Necessary
;(package-initialize)

(eval-and-compile
  ;; Mac : change modifiers
  (when (string= system-type "darwin")
    (setq mac-option-modifier 'none)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'super))

  (mapc (lambda (path)
	  (add-to-list 'load-path path))
	(directory-files
	 (expand-file-name "elpa" user-emacs-directory) t "^[^.].*-[0-9-]+")))


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
