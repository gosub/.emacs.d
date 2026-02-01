;;; gg-utils.el --- General utility functions -*- lexical-binding: t; -*-

(defun gg/get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(provide 'gg-utils)
;;; gg-utils.el ends here
