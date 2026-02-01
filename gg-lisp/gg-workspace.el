;;; gg-workspace.el --- Workspace setup -*- lexical-binding: t; -*-

(defvar gg-txt-directory (expand-file-name "~/box/txt")
  "Directory containing personal text files.")

(defvar gg-notes-file
  (expand-file-name "ziba.org" gg-txt-directory)
  "Path to personal notes org file.")

(defvar gg-todo-file
  (expand-file-name "todo.org" gg-txt-directory)
  "Path to todo org file.")

(defvar gg-done-file
  (expand-file-name "done.org" gg-txt-directory)
  "Path to done/archive org file.")

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
