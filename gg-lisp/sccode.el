;;; sccode.el --- Browse and dump sccode.org snippets -*- lexical-binding: t -*-

;; Author: you
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: supercollider, music
;; URL: https://sccode.org

;;; Commentary:
;;
;; Two commands:
;;
;;   M-x sccode-pick   -> open a random snippet in a new buffer
;;   M-x sccode-dump   -> sync new snippets from sccode.org into the local DB
;;
;; Customize `sccode-db-path' to point to your SQLite database.
;; `sclang-mode' is activated on pick buffers if available.
;;
;; Dump strategy (two phases):
;;   1. Fetch /api/code?page=N until a page contains only known IDs.
;;   2. For each new ID fetch /1-XXXX/json and store in SQLite.
;;
;; HTTP is done with url.el (built-in). DB with sqlite.el (built-in, Emacs 29).

;;; Code:

(require 'sqlite)
(require 'url)
(require 'url-http)
(require 'json)

;; -------------------------------------------------------------------------- ;;
;; Customization
;; -------------------------------------------------------------------------- ;;

(defgroup sccode nil
  "Browse and sync sccode.org snippets."
  :group 'tools
  :prefix "sccode-")

(defcustom sccode-db-path
  (expand-file-name "sccode.sqlite" user-emacs-directory)
  "Path to the sccode SQLite database."
  :type 'file
  :group 'sccode)

(defcustom sccode-delay-min 1.0
  "Minimum delay between HTTP requests during dump (seconds)."
  :type 'float
  :group 'sccode)

(defcustom sccode-delay-max 2.5
  "Maximum delay between HTTP requests during dump (seconds)."
  :type 'float
  :group 'sccode)

;; -------------------------------------------------------------------------- ;;
;; DB helpers
;; -------------------------------------------------------------------------- ;;

(defconst sccode--schema "
CREATE TABLE IF NOT EXISTS codes (
    id          TEXT PRIMARY KEY,
    id_int      INTEGER UNIQUE,
    fetched_at  TEXT,
    status      INTEGER,
    data        TEXT
);
CREATE TABLE IF NOT EXISTS meta (
    key   TEXT PRIMARY KEY,
    value TEXT
);
CREATE VIEW IF NOT EXISTS v_codes AS
SELECT
    id,
    id_int,
    fetched_at,
    json_extract(data, '$.author')        AS author,
    json_extract(data, '$.name')          AS name,
    json_extract(data, '$.code')          AS code,
    json_extract(data, '$.description')   AS description,
    json_extract(data, '$.labels')        AS labels,
    json_extract(data, '$.is_private')    AS is_private,
    json_extract(data, '$.ancestor_list') AS ancestor_list
FROM codes
WHERE status = 200;
")

(defun sccode--open-db ()
  "Open the sccode SQLite DB, creating schema if needed."
  (let ((db (sqlite-open sccode-db-path)))
    ;; executescript not available; run each statement separately.
    ;; Split on semicolons, skip empty strings.
    (dolist (stmt (split-string sccode--schema ";"))
      (let ((s (string-trim stmt)))
        (unless (string-empty-p s)
          ;; CREATE VIEW fails if view exists; ignore that error.
          (condition-case nil
              (sqlite-execute db s)
            (error nil)))))
    db))

(defun sccode--known-ids (db)
  "Return a hash-table of all IDs already in the DB."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (row (sqlite-select db "SELECT id FROM codes"))
      (puthash (car row) t ht))
    ht))

(defun sccode--b62-decode (s)
  "Decode base62 string S to an integer."
  (let ((alphabet "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (n 0))
    (mapc (lambda (c)
            (setq n (+ (* n 62) (string-search (char-to-string c) alphabet))))
          s)
    n))

(defun sccode--id-to-int (code-id)
  "Convert '1-5iN' to its integer value."
  (sccode--b62-decode (cadr (split-string code-id "-"))))

(defun sccode--upsert (db code-id status data)
  "Insert or replace a row in the codes table."
  (sqlite-execute
   db
   "INSERT OR REPLACE INTO codes(id, id_int, fetched_at, status, data)
    VALUES (?, ?, ?, ?, ?)"
   (list code-id
         (sccode--id-to-int code-id)
         (format-time-string "%Y-%m-%dT%T%z" nil t)
         status
         data)))

;; -------------------------------------------------------------------------- ;;
;; HTTP helpers
;; -------------------------------------------------------------------------- ;;

(defun sccode--get (url)
  "Fetch URL synchronously.  Return (STATUS . BODY-STRING) or nil on error."
  (let ((url-user-agent "sccode-archiver/1.0 (personal archival; be nice)")
        (url-request-extra-headers '(("Accept" . "application/json")))
        ;; suppress all url.el noise
        (url-show-status nil))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t t 15)
          (let ((status (url-http-parse-response)))
            (goto-char (point-min))
            ;; skip HTTP headers
            (re-search-forward "^\015?\n" nil t)
            (cons status (buffer-substring-no-properties (point) (point-max)))))
      (error
       (message "sccode: network error for %s: %s" url (error-message-string err))
       nil))))

(defun sccode--random-delay ()
  "Sleep a random amount between `sccode-delay-min' and `sccode-delay-max'."
  (sleep-for (+ sccode-delay-min
                (random (truncate (* 1000 (- sccode-delay-max sccode-delay-min))))
                0.001)))

;; -------------------------------------------------------------------------- ;;
;; Dump: phase 1 -> listing
;; -------------------------------------------------------------------------- ;;

(defun sccode--collect-new-ids (known log-buf)
  "Fetch listing pages until all IDs on a page are known.
KNOWN is a hash-table.  LOG-BUF is the buffer for progress output.
Returns a list of new IDs ordered oldest-first."
  (let ((new-ids '())
        (page 1)
        (done nil))
    (while (not done)
      (let* ((url (format "https://sccode.org/api/code?page=%d" page))
             (result (sccode--get url)))
        (cond
         ((null result)
          (sccode--log log-buf "  network error on page %d, stopping.\n" page)
          (setq done t))
         ((not (= (car result) 200))
          (sccode--log log-buf "  page %d: status %d, done.\n" page (car result))
          (setq done t))
         (t
          (let* ((entries (condition-case nil
                              (json-parse-string (cdr result)
                                                 :array-type 'list
                                                 :object-type 'alist)
                            (error nil))))
            (cond
             ((null entries)
              (sccode--log log-buf "  page %d: no entries (invalid JSON or empty page), stopping.\n" page)
              (setq done t))
             (t
              (let* ((page-ids (delq nil (mapcar (lambda (e)
                                                   (alist-get 'id e))
                                                 entries)))
                     (new-on-page (seq-remove (lambda (id)
                                                (gethash id known))
                                              page-ids)))
                (sccode--log log-buf "  page %4d: %2d entries, %2d new\n"
                             page (length page-ids) (length new-on-page))
                (setq new-ids (append new-ids new-on-page))
                (if (null new-on-page)
                    (progn
                      (sccode--log log-buf "  all known, listing complete.\n")
                      (setq done t))
                  (setq page (1+ page))
                  (sccode--random-delay)))))))))
    ;; API returns newest-first; reverse for chronological fetch order.
    (nreverse new-ids))))

;; -------------------------------------------------------------------------- ;;
;; Dump: phase 2 -> fetch details
;; -------------------------------------------------------------------------- ;;

(defun sccode--fetch-details (ids db log-buf)
  "Fetch /1-XXXX/json for each id in IDS and store in DB."
  (let ((total (length ids))
        (fetched 0)
        (errors 0)
        (consecutive 0)
        (max-consecutive 10))
    (sccode--log log-buf "\nPhase 2: fetching details for %d snippets...\n\n" total)
    (catch 'abort
      (seq-do-indexed
       (lambda (code-id i)
         (let* ((url (format "https://sccode.org/%s/json" code-id))
                (result (sccode--get url))
                (status (if result (car result) 0))
                (body   (if result (cdr result) nil)))
           (cond
            ((= status 200)
             (sccode--upsert db code-id 200 body)
             (setq fetched (1+ fetched) consecutive 0)
             (sccode--log log-buf "  ok  %-12s (%d/%d)\n" code-id (1+ i) total))
            ((memq status '(404 410))
             (sccode--upsert db code-id status nil)
             (setq consecutive 0)
             (sccode--log log-buf "  --  %-12s %d\n" code-id status))
            (t
             (setq consecutive (1+ consecutive) errors (1+ errors))
             (sccode--log log-buf "  !!  %-12s status=%d (consecutive errors: %d)\n"
                          code-id status consecutive)
             (when (>= consecutive max-consecutive)
               (sccode--log log-buf "\nToo many consecutive errors, aborting.\n")
               (throw 'abort nil))))
           (sccode--random-delay)))
       ids))
    (sccode--log log-buf "\nDone: %d fetched, %d errors out of %d total.\n"
                 fetched errors total)))

;; -------------------------------------------------------------------------- ;;
;; Logging
;; -------------------------------------------------------------------------- ;;

(defun sccode--log (buf fmt &rest args)
  "Append formatted message to BUF and move point to end."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert (apply #'format fmt args))))

;; -------------------------------------------------------------------------- ;;
;; Pick: header formatting
;; -------------------------------------------------------------------------- ;;

(defun sccode--format-header (fields)
  "Format FIELDS alist as SuperCollider // comments.
Continuation lines of multi-line values are indented."
  (mapconcat
   (lambda (pair)
     (let* ((k (car pair))
            (v (cdr pair))
            (lines (when (and v (not (string-empty-p v)))
                     (split-string v "\n"))))
       (when lines
         (concat
          (format "// %s: %s" k (car lines))
          (when (cdr lines)
            (concat "\n"
                    (mapconcat (lambda (l) (format "//         %s" l))
                               (cdr lines) "\n")))))))
   (seq-filter (lambda (p) (and (cdr p) (not (string-empty-p (cdr p)))))
               fields)
   "\n"))

(defun sccode--open-snippet (id author name description labels code)
  "Open a new buffer with snippet metadata as comments followed by CODE."
  (let* ((bufname (format "*sccode:%s*" id))
         (header  (sccode--format-header
                   `(("id"          . ,id)
                     ("author"      . ,(or author ""))
                     ("title"       . ,(or name ""))
                     ("description" . ,(or description ""))
                     ("tags"        . ,(or labels "")))))
         (content (concat header "\n\n" code)))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (if (fboundp 'sclang-mode)
          (sclang-mode)
        (message "sccode: sclang-mode not found, using fundamental-mode"))
      (set-buffer-modified-p nil)
      (pop-to-buffer (current-buffer)))))

;; -------------------------------------------------------------------------- ;;
;; Public commands
;; -------------------------------------------------------------------------- ;;

;;;###autoload
(defun sccode-pick (&optional id)
  "Open a random sccode.org snippet in a new buffer.
With prefix argument, prompt for a specific snippet ID."
  (interactive
   (list (when current-prefix-arg
           (read-string "Snippet ID (e.g. 1-5iN): "))))
  (let* ((db  (sccode--open-db))
         (row (if id
                  (sqlite-select
                   db
                   "SELECT id, author, name, description, labels, code
                    FROM v_codes WHERE id = ? AND code IS NOT NULL"
                   (list id))
                (sqlite-select
                 db
                 "SELECT id, author, name, description, labels, code
                  FROM v_codes WHERE code IS NOT NULL ORDER BY RANDOM() LIMIT 1"))))
    (sqlite-close db)
    (if (null row)
        (user-error "sccode: no snippet found%s"
                    (if id (format " for id %s" id) ""))
      (apply #'sccode--open-snippet (car row)))))

;;;###autoload
(defun sccode-dump ()
  "Sync new snippets from sccode.org into the local DB.
Runs synchronously in a *sccode-dump* buffer with live progress."
  (interactive)
  (let* ((buf (get-buffer-create "*sccode-dump*"))
         (db  (sccode--open-db))
         (known (sccode--known-ids db)))
    (with-current-buffer buf (erase-buffer))
    (pop-to-buffer buf)
    (sccode--log buf "IDs already in DB: %d\n\n" (hash-table-count known))
    (sccode--log buf "Phase 1: collecting new IDs from listing API...\n")
    (let ((new-ids (sccode--collect-new-ids known buf)))
      (sccode--log buf "\nNew IDs found: %d\n" (length new-ids))
      (if (null new-ids)
          (sccode--log buf "Nothing to do.\n")
        (sccode--fetch-details new-ids db buf)))
    (sqlite-close db)
    (sccode--log buf "\nAll done.\n")))

(provide 'sccode)
;;; sccode.el ends here
