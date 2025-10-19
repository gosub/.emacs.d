;;; Graveyard - a plethora of functions I don't use anymore


; my idea is to substitute this two next functions
; with some custom keybindings in isearch-mode
; that marks a thing-at-point (word,line,paragraph)
; copy it, and paste it where the search started

(defun gg/search-forward-and-copy-line ()
  "search forwards and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-forward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


(defun gg/search-backward-and-copy-line ()
  "search backward and insert whole line found at point"
  (interactive)
  (let ((p (point)))
    (when (not (null (isearch-backward)))
      (move-beginning-of-line nil)
      (kill-line)
      (yank)
      (goto-char p)
      (yank))))


;; search and duplicate whole line
;(global-set-key (kbd "C-c s")
;		'gg/search-forward-and-copy-line)
;(global-set-key (kbd "C-c r")
;		'gg/search-backward-and-copy-line)


(defun gg/yank-line-at-beginning-of-buffer ()
  "yank the content of the clipboard at the beginning of the buffer"
  (interactive)
  (beginning-of-buffer)
  (yank)
  (newline))


;; yank at beginning of buffer
;(global-set-key (kbd "C-c y")
;		'gg/yank-line-at-beginning-of-buffer)

(provide 'gg-graveyard)
