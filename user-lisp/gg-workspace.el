;;; gg-workspace.el --- Workspace setup -*- lexical-binding: t; -*-

(defvar gg-txt-directory (expand-file-name "~/dl/io/org")
  "Directory containing the live text files, synced with the phone.
The history lives in ~/box/txt; see `riporta-gg'.")

(defvar gg-notes-file
  (expand-file-name "misc.org" gg-txt-directory)
  "Path to personal notes org file.")

(defvar gg-todo-file
  (expand-file-name "todo.org" gg-txt-directory)
  "Path to todo org file.")

(defvar gg-done-file
  (expand-file-name "io.org" gg-txt-directory)
  "Path to the diary of the current month.")

;;;###autoload
(defun gg/apparecchia ()
  "Setup initial buffers and windows as I like them."
  (interactive)
  (eshell)
  (split-window-right)
  (other-window 1)
  (find-file gg-notes-file)
  (find-file gg-todo-file)
  (find-file gg-done-file))

(provide 'gg-workspace)
;;; gg-workspace.el ends here
