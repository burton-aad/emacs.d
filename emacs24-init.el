;; Initialisation spécifique pour emacs 24


(if (> emacs-minor-version 3)
    (defmacro define-advice (cmd arglist &rest body)
      "Macro de substitution de define-advice pour emacs 24"
      `(progn
	     (defun ,(car (last arglist)) ,(cadr arglist)
	       ,@body)
	     (advice-add (quote ,cmd) ,(car arglist) #',(car (last arglist)))))
  ;; Emacs 24.3 start to be quite old so we need to setup some stuff
  (defmacro define-advice (&rest args)
    "Defining advice have changed a lot after emacs 24.3 so for now this macro does nothing."
    nil)
  ;; On utilise pas ibuffer puisque le advice ne marche pas. Du coup on
  ;; retourne sur buffer-menu
  (global-set-key (kbd "C-x C-b") 'buffer-menu)
  )
