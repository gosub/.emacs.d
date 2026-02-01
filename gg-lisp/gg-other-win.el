;;; gg-other-win.el -*- lexical-binding: t; -*-

(defun gg/kill-buffer-and-windows-other-window ()
  "Kill the buffer and the window of the other window."
  (interactive)
  (if (one-window-p)
      (kill-current-buffer)
    (other-window 1)
    (kill-buffer-and-window)))

(provide 'gg-other-win)
