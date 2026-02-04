;;; gg-mark-whole.el --- Mark whole text objects under cursor -*- lexical-binding: t; -*-

(require 'thingatpt)
(require 'cl-lib)

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

(defun gg-mark-whole-line-with-newline ()
  "Mark the whole line under the cursor including trailing newline."
  (interactive)
  (beginning-of-line)
  (push-mark (line-beginning-position 2) nil t))

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

(defun gg-mark-whole-number ()
  "Mark the whole number under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'number)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No number at point"))))

(defun gg-mark-whole-email ()
  "Mark the whole email address under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'email)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No email at point"))))

(defun gg-mark-whole-filename ()
  "Mark the whole filename under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No filename at point"))))

(defun gg-mark-whole-whitespace ()
  "Mark the whole whitespace under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'whitespace)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No whitespace at point"))))

(defun gg-mark-whole-url ()
  "Mark the whole URL under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (if bounds
        (progn
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))
      (message "No URL at point"))))

(defvar gg-mark-whole-thing-types
  '(word symbol list sexp defun line sentence paragraph
    page whitespace filename url email uuid number)
  "List of thing types to check in `gg-mark-whole-dwim'.")

(defun gg-mark-whole-dwim ()
  "Mark a thing at point, prompting for which type to mark.
Only shows thing types that are actually present at point."
  (interactive)
  (let* ((available
          (cl-remove-if-not
           (lambda (type) (bounds-of-thing-at-point type))
           gg-mark-whole-thing-types))
         (choice (if available
                     (intern (completing-read "Mark whole: "
                                              (mapcar #'symbol-name available)
                                              nil t))
                   (user-error "No thing at point")))
         (bounds (bounds-of-thing-at-point choice)))
    (goto-char (car bounds))
    (push-mark (cdr bounds) nil t)))

(provide 'gg-mark-whole)
;;; gg-mark-whole.el ends here
