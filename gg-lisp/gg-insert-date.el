;;; gg-insert-date.el --- Insert current date -*- lexical-binding: t; -*-

(require 'gg-utils)

(defcustom gg/insert-date-formats '("%F")
  "Time string formats to choose from via successive C-u."
  :type '(repeat string)
  :group 'gg)

(defun gg/insert-current-date (arg)
  "Insert current date at point.
Format the date according to `gg/insert-date-formats'.
Use C-u to cycle through formats."
  (interactive "p")
  (let* ((ua (gg/count-universal-arguments arg))
         (idx (min ua (1- (length gg/insert-date-formats))))
         (fmt (nth idx gg/insert-date-formats)))
    (insert (format-time-string fmt))))

(provide 'gg-insert-date)
;;; gg-insert-date.el ends here
