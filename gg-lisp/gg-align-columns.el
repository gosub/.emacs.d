;;; gg-align-columns.el -*- lexical-binding: t; -*-

(defun gg/align-columns-by-whitespace (spacing)
  (interactive "p")
  (when (region-active-p)
    (let ((align-to-tab-stop nil)
          (indent-tabs-mode nil))
      (align-regexp
       (region-beginning) (region-end)
       (rx (group (+ (syntax whitespace))))
       1 spacing t))))

(provide 'gg-align-columns)
