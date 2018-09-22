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
  (setq el-get-sources
        '((:name git-modes-1.0.0
                 :description "dependency for magit 1.4.2"
                 :type github
                 :pkgname "magit/git-modes"
                 :checkout "1.0.0")
          (:name magit-1.4.2
                 :description "magit version 1.4.2 compatible with emacs 24.3"
                 :type github
                 :pkgname "magit/magit"
                 :depends git-modes-1.0.0
                 :checkout "1.4.2")))
  ;; magit 1.4.2 need a special setup
  (setq magit-last-seen-setup-instructions "1.4.0")
  (let ((el-get (file-name-as-directory (expand-file-name "el-get" user-emacs-directory))))
    (add-to-list 'load-path (concat el-get "git-modes-1.0.0"))
    (add-to-list 'load-path (concat el-get "magit-1.4.2")))
  )
