;;; gg-mark-whole.el --- Mark whole text objects under cursor -*- lexical-binding: t; -*-

(require 'thingatpt)

(defun gg-mark-whole-word ()
  "Mark the whole word under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No word at point"))))

(defun gg-mark-whole-line ()
  "Mark the whole line under the cursor."
  (interactive)
  (beginning-of-line)
  (push-mark (line-end-position) nil t))

(defun gg-mark-whole-paragraph ()
  "Mark the whole paragraph under the cursor."
  (interactive)
  (mark-paragraph))

(defun gg-mark-whole-function ()
  "Mark the whole function (defun) under the cursor."
  (interactive)
  (mark-defun))

(defun gg-mark-whole-buffer ()
  "Mark the entire buffer."
  (interactive)
  (mark-whole-buffer))

(provide 'gg-mark-whole)
;;; gg-mark-whole.el ends here
