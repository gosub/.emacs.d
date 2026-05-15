;;; project-aux.el --- Jump to git repos under a base directory -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "28.1"))

;;; Code:

(require 'subr-x)

(defgroup project-aux nil
  "Navigate git repositories under a base directory."
  :group 'tools
  :prefix "project-aux-")

(defcustom project-aux-base-dir "~/box/prj"
  "Root directory to search for git repositories."
  :type 'directory
  :group 'project-aux)

(defcustom project-aux-search-depth 4
  "Maximum subdirectory depth when searching for git repositories."
  :type 'integer
  :group 'project-aux)

(defvar project-aux--cache nil
  "Cached repo list. Cleared by `project-aux-clear-cache' or with \\[universal-argument].")

(defun project-aux-clear-cache ()
  "Clear the cached repository list."
  (interactive)
  (setq project-aux--cache nil)
  (message "project-aux: cache cleared"))

(defun project-aux--find-repos ()
  "Return list of git repo root paths under `project-aux-base-dir'."
  (let* ((base (expand-file-name project-aux-base-dir))
         (raw  (shell-command-to-string
                (format "find %s -maxdepth %d -name .git -type d 2>/dev/null"
                        (shell-quote-argument base)
                        project-aux-search-depth))))
    (mapcar #'file-name-directory
            (seq-filter (lambda (s) (not (string-empty-p s)))
                        (split-string raw "\n")))))

;;;###autoload
(defun project-aux-open (arg)
  "Select a git repo under `project-aux-base-dir' and open it in dired.
With prefix argument ARG, refresh the repo list first."
  (interactive "P")
  (when arg (setq project-aux--cache nil))
  (unless project-aux--cache
    (message "project-aux: scanning %s..." project-aux-base-dir)
    (setq project-aux--cache (project-aux--find-repos)))
  (when (null project-aux--cache)
    (user-error "project-aux: no git repos found under %s" project-aux-base-dir))
  (let* ((base    (file-name-as-directory (expand-file-name project-aux-base-dir)))
         (choices (mapcar (lambda (r)
                            (cons (directory-file-name (string-remove-prefix base r))
                                  r))
                          project-aux--cache))
         (key     (completing-read "Project: " (mapcar #'car choices) nil t))
         (path    (cdr (assoc key choices))))
    (dired path)))

(provide 'project-aux)
;;; project-aux.el ends here
