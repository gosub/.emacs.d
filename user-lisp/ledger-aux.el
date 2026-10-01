;;; ledger-aux.el --- Helpers for ledger files split with include -*- lexical-binding: t; -*-

;;; Commentary:

;; ledger-mode completes accounts and payees from the postings of the
;; current buffer.  When the journal is a master file that only includes
;; yearly files, the master has no postings and a yearly file has only
;; its own year.  `ledger-aux-setup' makes completion ask ledger itself,
;; which follows the includes, and gives every file of the journal the
;; same master file, so that balances cover the whole history.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar ledger-master-file)
(defvar ledger-binary-path)
(declare-function ledger-master-file "ledger-report" ())


(defgroup ledger-aux nil
  "Helpers for ledger files split with include."
  :group 'ledger)

(defcustom ledger-aux-master-files nil
  "Alist of (PLACE . MASTER) for `ledger-aux-set-master'.
PLACE is a directory, meaning every file below it, or a file.  Buffers
visiting it get MASTER as their `ledger-master-file'."
  :type '(alist :key-type (choice directory file) :value-type file)
  :group 'ledger-aux)


(defun ledger-aux-set-master ()
  "Set `ledger-master-file' from `ledger-aux-master-files'."
  (when-let* ((file (buffer-file-name))
              (entry (cl-find-if
                      (lambda (e)
                        (let ((place (expand-file-name (car e))))
                          (if (directory-name-p (car e))
                              (file-in-directory-p file place)
                            (file-equal-p file place))))
                      ledger-aux-master-files)))
    (unless (file-equal-p file (cdr entry))
      (setq-local ledger-master-file (expand-file-name (cdr entry))))))

(defun ledger-aux--uses-includes-p ()
  "Return non-nil if the journal of this buffer spans several files."
  (or (bound-and-true-p ledger-master-file)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^include[ \t]" nil t))))

(defun ledger-aux--ledger-list (command)
  "Return the lines of `ledger COMMAND' on the master file, or nil."
  (let ((master (ledger-master-file)))
    (when (and master (file-exists-p master))
      (ignore-errors
        (process-lines ledger-binary-path "-f" master command)))))

(defun ledger-aux--accounts (orig)
  "Around advice for `ledger-accounts-list' calling ORIG as a fallback."
  (or (and (not (bound-and-true-p ledger-accounts-file))
           (ledger-aux--uses-includes-p)
           (ledger-aux--ledger-list "accounts"))
      (funcall orig)))

(defun ledger-aux--payees (orig)
  "Around advice for `ledger-payees-list' calling ORIG as a fallback."
  (or (and (not (bound-and-true-p ledger-payees-file))
           (ledger-aux--uses-includes-p)
           (ledger-aux--ledger-list "payees"))
      (funcall orig)))

;;;###autoload
(defun ledger-aux-setup ()
  "Install the ledger-aux helpers into ledger-mode."
  (add-hook 'ledger-mode-hook #'ledger-aux-set-master)
  (advice-add 'ledger-accounts-list :around #'ledger-aux--accounts)
  (advice-add 'ledger-payees-list :around #'ledger-aux--payees))

(provide 'ledger-aux)
;;; ledger-aux.el ends here
