;;; Insert current date


(defcustom gg/insert-date-format-list
  '("%F")
  "List of date-time format to be fed to `format-time-string'")


;; TODO: docstring
;; TODO: generalize universal arg counting

(defun gg/insert-current-date (arg)
  "Insert current date"
  (interactive "p")
  (let* ((fmts gg/insert-date-format-list)
	(fmt (cl-case arg
	       (16 (caddr fmts))
	       (4 (cadr fmts))
	       (t (car fmts)))))
    (insert (format-time-string fmt))))


(provide 'gg-insert-date)
