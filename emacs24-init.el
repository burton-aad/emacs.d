;; Initialisation spécifique pour emacs 24

(defmacro define-advice (cmd arglist &rest body)
  "Macro de substitution de define-advice  pour emacs 24"
  `(progn
	 (defun ,(car (last arglist)) ,(cadr arglist)
	   ,@body)
	 (advice-add (quote ,cmd) ,(car arglist) #',(car (last arglist)))))
