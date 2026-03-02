;;; dirty-gits.el --- Browse git repos with uncommitted changes -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)


(defgroup dirty-gits nil
  "Browse git repositories with uncommitted changes."
  :group 'tools)

(defcustom dirty-gits-max-depth 3
  "Maximum depth of repositories relative to the root directory."
  :type 'natnum
  :group 'dirty-gits)

(defcustom dirty-gits-root nil
  "Default root directory to scan for git repositories.
When non-nil and `dirty-gits' is called without a prefix argument,
this directory is used directly without prompting."
  :type '(choice directory (const nil))
  :group 'dirty-gits)


(defun dirty-gits--find-repos (root)
  "Return a list of git repo paths under ROOT up to `dirty-gits-max-depth'."
  (let* ((maxdepth (1+ dirty-gits-max-depth))
         (output (shell-command-to-string
                  (format "find %s -maxdepth %d -name .git -type d"
                          (shell-quote-argument (expand-file-name root))
                          maxdepth))))
    (delq nil
          (mapcar (lambda (line)
                    (when (string-suffix-p "/.git" line)
                      (file-name-directory line)))
                  (split-string output "\n" t)))))

(defun dirty-gits--git (repo &rest args)
  "Run git in REPO with ARGS; return trimmed stdout, or nil on error."
  (let ((default-directory repo))
    (with-temp-buffer
      (when (= 0 (apply #'call-process "git" nil t nil args))
        (string-trim (buffer-string))))))

(defun dirty-gits--status-string (repo)
  "Return a change-summary string for REPO, or nil if the repo is clean."
  (let ((raw (dirty-gits--git repo "status" "--porcelain")))
    (when (and raw (not (string-empty-p raw)))
      (let ((staged 0) (unstaged 0) (untracked 0))
        (dolist (line (split-string raw "\n" t))
          (when (>= (length line) 2)
            (let ((x (aref line 0))
                  (y (aref line 1)))
              (cond
               ((and (eq x ??) (eq y ??)) (cl-incf untracked))
               (t
                (when (not (eq x ?\s)) (cl-incf staged))
                (when (not (eq y ?\s)) (cl-incf unstaged)))))))
        (string-join
         (delq nil
               (list (when (> staged    0) (format "staged:%d"    staged))
                     (when (> unstaged  0) (format "unstaged:%d"  unstaged))
                     (when (> untracked 0) (format "untracked:%d" untracked))))
         "  ")))))

(defun dirty-gits--entries (root)
  "Build `tabulated-list-entries' for dirty repos under ROOT."
  (let ((root (file-truename (expand-file-name root))))
    (delq nil
          (mapcar
           (lambda (repo)
             (let ((status (dirty-gits--status-string repo)))
               (when status
                 (let* ((branch (or (dirty-gits--git repo "rev-parse"
                                                      "--abbrev-ref" "HEAD")
                                    "?"))
                        (rel (file-relative-name (directory-file-name repo)
                                                 root)))
                   (list repo
                         (vector rel branch status))))))
           (dirty-gits--find-repos root)))))


(defvar-local dirty-gits--root nil
  "Root directory used by the current `dirty-gits-mode' buffer.")

(defun dirty-gits-open ()
  "Open the repository at point in Dired."
  (interactive)
  (let ((repo (tabulated-list-get-id)))
    (if repo
        (dired repo)
      (user-error "No repository at point"))))

(defun dirty-gits-refresh ()
  "Re-scan the root directory and redraw the buffer."
  (interactive)
  (unless dirty-gits--root
    (user-error "No root directory recorded for this buffer"))
  (setq tabulated-list-entries (dirty-gits--entries dirty-gits--root))
  (tabulated-list-print t))


(define-derived-mode dirty-gits-mode tabulated-list-mode "Dirty-Gits"
  "Major mode for listing git repositories with uncommitted changes."
  (setq tabulated-list-format
        [("Repository" 45 t)
         ("Branch"     15 t)
         ("Changes"     0 nil)])
  (setq tabulated-list-sort-key '("Repository" . nil))
  (tabulated-list-init-header)
  (define-key dirty-gits-mode-map (kbd "RET") #'dirty-gits-open)
  (define-key dirty-gits-mode-map (kbd "g")   #'dirty-gits-refresh))


;;;###autoload
(defun dirty-gits (root)
  "List git repositories with uncommitted changes under ROOT.
Without a prefix argument, use `dirty-gits-root' if set; otherwise prompt.
With a prefix argument, always prompt."
  (interactive
   (list (if (and dirty-gits-root (not current-prefix-arg))
             dirty-gits-root
           (read-directory-name "Root directory: " dirty-gits-root))))
  (let ((buf (get-buffer-create "*dirty-gits*")))
    (with-current-buffer buf
      (dirty-gits-mode)
      (setq dirty-gits--root (file-truename (expand-file-name root)))
      (setq tabulated-list-entries (dirty-gits--entries root))
      (tabulated-list-print))
    (switch-to-buffer buf)))


(provide 'dirty-gits)
;;; dirty-gits.el ends here
