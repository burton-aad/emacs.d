
(defun my/max_buffer_len ()
  "Return the maximum buffer name length.
   It ignores the special buffers from emacs (\"*buffer*\")"
  (apply 'max (mapcar 'length
    (cl-remove-if (lambda (name) (string-match-p " ?\\*.*\\*" name))
				  (mapcar 'buffer-name (buffer-list)))
	)))

(defun my/ibuffer_name_len ()
  "Special ibuffer function for name min/max size"
  (interactive)
  (let ((max_buf_len (my/max_buffer_len)))
	(max 18 max_buf_len)
	))

(setq my/min_length 16)
(setq my/bufs_max_length 0) ;; init at 0
(define-ibuffer-column name-left-aligned
  (:name "Name" 
   :inline nil
   :header-mouse-map ibuffer-name-header-map
   :props
   ('mouse-face 'highlight 'keymap ibuffer-name-map
		'ibuffer-name-column t
		'help-echo '(if tooltip-mode
				"mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
			      "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
   :summarizer
   (lambda (strings)
     (let ((bufs (length strings)))
	   (setq my/bufs_max_length 0) ;; reset value
       (cond ((zerop bufs) "No buffers")
	     ((= 1 bufs) "1 buffer")
	     (t (format "%s buffers" bufs))))))
  (when (= my/bufs_max_length 0)
	(setq my/bufs_max_length (max my/min_length (my/max_buffer_len)))
	(setq my/bufs_format (format "%%-%ds" my/bufs_max_length)))
  (propertize (format my/bufs_format (buffer-name)) 'font-lock-face (ibuffer-buffer-name-face buffer mark)))

;; (my/ibuffer_name_len)
;; (debug-on-entry 'number-or-marker-p)
;; (debug-on-entry 'wrong-type-argument)
;; (eval((my/ibuffer_name_len) nil))
;; (setq debug-on-error t)

;; (setq ibuffer-formats
;; 	  (quote
;; 	   ((mark modified read-only " "
;; 			  (name
;; 			   (my/ibuffer_name_len)
;; 			   -1 :left :elide)
;; 			  " "
;; 			  (size 9 -1 :right)
;; 			  " "
;; 			  (mode 16 16 :left :elide)
;; 			  " " filename-and-process))))
