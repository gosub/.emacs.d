;;; gg-isearch-pop-yank.el -*- lexical-binding: t; -*-

(defun isearch-mark-whole-paragraph-pop-mark-and-yank ()
  (interactive)
  (isearch-exit)
  (let ((pos isearch-opoint))
    (mark-paragraph)
    (kill-ring-save 0 0 t)
    (goto-char pos)
    (yank)))

(provide 'gg-isearch-pop-yank)
