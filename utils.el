

;; Mesure le temps du code passé en paramètre
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))


;; Mode pour voir les couleurs ansi dans le buffer
;; (si le buffer est trop grand ça peut être long)
(define-derived-mode fundamental-ansi-mode fundamental-mode "fundamental ansi"
  "Fundamental mode that understands ansi colors."
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))


;; Ajout d'une confirmation pour fermer Emacs (à placer dans init si nécessaire)
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really exit Emacs? "))
            kill-emacs-query-functions))


;; Fonction pour ouvrir un buffer vide sans nom
(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "C-c C-n") 'xah-new-empty-buffer)


;; Function pour remplacer une sexp par son evaluation
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)


;; Fonction pour changer du camel case en petit avec underscore
(defun cc2sc ()
  "camel case to small case with underscore : TotoTata -> toto_tata"
  (interactive)
  (save-excursion
	(let ( (end (progn
				  (forward-word)
				  (point)))
		   (beg (progn
				  (backward-word)
				  (point))) )
	  (message (format "word boudary %d %d" beg end))
	  (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ beg) end)
	  (downcase-region beg (cdr (bounds-of-thing-at-point 'symbol)))
	  )))


;; Special file to write in kill ring
;; from http://emacs.stackexchange.com/questions/30458/how-to-send-shell-stdout-directly-to-kill-ring
;;
;; In Eshell, you can save output to the kill ring by redirecting output
;; to /dev/kill. You can do the similar in any shell:
;;
;;   - redirect output to /tmp/kill
;;   - when the contents of /tmp/kill changes, make Emacs copies the
;;     new contents to the kill ring
;;
;; (notes that your Emacs should be compiled with support of watching filesystem,
;; I guess it means (require 'filenotify) should return non-nil)
(when (require 'filenotify nil t)
  ;; make different file for each users and each emacs
  (setq my-killsavefile (format "/tmp/emacs%d/kill%d" (user-uid) (emacs-pid)))

  (defun my-file-contents (file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (buffer-string)))

  (defun save-to-kill-ring-if-changes (event)
	;; (message "Event %S" event)
	(when (eq (cadr event) 'changed)
	  (kill-new (my-file-contents my-killsavefile))))

  ;; Create /tmp/kill firstly
  (write-region "" nil my-killsavefile)
  (file-notify-add-watch my-killsavefile '(change) 'save-to-kill-ring-if-changes)
  )
;; then in any shell, % foo > /tmp/kill should copy the output of foo to the kill ring.

