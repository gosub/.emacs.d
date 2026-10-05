;;; riporta-gg.el --- Move entries from the phone folder to the archive -*- lexical-binding: t; -*-

;;; Commentary:

;; The phone folder (`riporta-gg-io-dir', synced with Syncthing) holds
;; the live files: a diary, a log of consumed media, a ledger of expenses
;; and the todo lists.  The archive (`riporta-gg-base-dir', a git repo)
;; holds the history, split by year.
;;
;; `riporta-gg' moves the entries that are ready from the phone files to
;; the yearly archive files, in the same format, then commits the archive.
;; Nothing is transcribed: an entry is appended to the archive, verified,
;; and only then removed from the phone file.
;;
;; Before touching anything it asks the local Syncthing REST API whether
;; the phone has been seen recently and whether the folder is in sync, so
;; that it never moves a stale copy of a file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'parse-time)
(require 'org)
(require 'org-archive)

(defvar url-http-response-status)


(defgroup riporta-gg nil
  "Move entries from the phone folder to the archive."
  :group 'files)

(defcustom riporta-gg-io-dir "~/io/"
  "Directory with the live files, synced with the phone."
  :type 'directory
  :group 'riporta-gg)

(defcustom riporta-gg-base-dir "~/archive/"
  "Directory of the archive, a git repository."
  :type 'directory
  :group 'riporta-gg)

(defcustom riporta-gg-streams
  '((diario "done.org" "done/%Y.org" org mesi-conclusi)
    (slurp "slurp.org" "slurp/%Y.org" org mesi-conclusi)
    (contab "contab.txt" "contab/%Y.ledger" ledger tutto))
  "Streams moved by `riporta-gg'.
Each element is (NAME IO-FILE DEST FORMAT POLICY):
- IO-FILE is relative to `riporta-gg-io-dir';
- DEST is relative to `riporta-gg-base-dir', with %Y replaced by the
  year of the entry;
- FORMAT is `org' (an entry is a top-level heading, dated by the last
  YYYY-MM-DD on the heading line) or `ledger' (an entry is a block of
  lines separated by blank lines, dated by its first YYYY-MM-DD);
- POLICY is `mesi-conclusi' (move the entries of the months before the
  current one) or `tutto' (move every dated entry).
Entries without a date are never moved."
  :type '(repeat (list symbol string string
                       (choice (const org) (const ledger))
                       (choice (const mesi-conclusi) (const tutto))))
  :group 'riporta-gg)

(defcustom riporta-gg-archive-files '("todo.org" "idee.org")
  "Files in `riporta-gg-io-dir' whose closed DONE items get archived."
  :type '(repeat string)
  :group 'riporta-gg)

(defcustom riporta-gg-mirror-files '("todo.org" "idee.org" "misc.org")
  "Files in `riporta-gg-io-dir' copied to `riporta-gg-base-dir' at every run.
The copies are read-only: they are edited on the phone only, and the
archive keeps their history in git."
  :type '(repeat string)
  :group 'riporta-gg)

(defcustom riporta-gg-archive-dest "completed/%s_%Y.org"
  "Archive file for DONE items, relative to `riporta-gg-base-dir'.
%s is replaced by the base name of the source file, %Y by the year of
the CLOSED timestamp."
  :type 'string
  :group 'riporta-gg)

(defcustom riporta-gg-archive-age 7
  "Archive DONE items closed at least this many days ago."
  :type 'natnum
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-url "http://127.0.0.1:8384"
  "Base URL of the local Syncthing REST API."
  :type 'string
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-config "~/.config/syncthing/config.xml"
  "Syncthing configuration file, read to get the API key."
  :type 'file
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-device-name nil
  "Name of the phone in the Syncthing configuration.
It must be set: without it the Syncthing checks fail."
  :type '(choice string (const nil))
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-folder-label nil
  "Label of the Syncthing folder that contains `riporta-gg-io-dir'.
When nil, the folder is the one whose path contains `riporta-gg-io-dir'."
  :type '(choice string (const nil))
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-max-age 15
  "Minutes since the phone was last seen beyond which nothing is moved."
  :type 'natnum
  :group 'riporta-gg)

(defcustom riporta-gg-syncthing-wait 30
  "Seconds to wait for the phone to receive the shortened files."
  :type 'natnum
  :group 'riporta-gg)


;;; Splitting files into chunks

(defconst riporta-gg--date-re
  "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-[0-9]\\{2\\}"
  "Regexp matching a YYYY-MM-DD date.")

(defun riporta-gg--org-chunk-date (chunk)
  "Return the last date on the first line of CHUNK if it is a heading."
  (when (string-prefix-p "* " chunk)
    (let ((line (car (split-string chunk "\n")))
          (start 0) date)
      (while (string-match riporta-gg--date-re line start)
        (setq date (match-string 0 line)
              start (match-end 0)))
      date)))

(defun riporta-gg--ledger-chunk-date (chunk)
  "Return the first date in CHUNK if it is a transaction.
A block made only of comments is dated by the first date it contains,
so that commented-out transactions travel with their year."
  (let ((lines (split-string chunk "\n" t)))
    (cond
     ((string-match (concat "\\`" riporta-gg--date-re) (car lines))
      (match-string 0 (car lines)))
     ((and (cl-every (lambda (l) (string-match-p "\\`[ \t]*[;#%|*]" l))
                     lines)
           (string-match riporta-gg--date-re chunk))
      (match-string 0 chunk)))))

(defun riporta-gg--split (text format)
  "Split TEXT into chunks according to FORMAT, `org' or `ledger'.
Return a list of (DATE . STRING), where DATE may be nil.  Concatenating
the strings gives back TEXT exactly."
  (let ((starts (list 0))
        (pos 0)
        (len (length text))
        (prev-blank t))
    (while (< pos len)
      (let* ((eol (or (string-search "\n" text pos) len))
             (line (substring text pos eol))
             (blank (not (string-match-p "[^ \t]" line))))
        (when (and (> pos 0)
                   (pcase format
                     ('org (string-prefix-p "* " line))
                     ('ledger (and prev-blank (not blank)))))
          (push pos starts))
        (setq prev-blank blank
              pos (min len (1+ eol)))))
    (setq starts (nreverse starts))
    (let ((chunks
           (cl-loop for (beg end) on starts
                    for s = (substring text beg (or end len))
                    unless (string-empty-p s)
                    collect (cons (pcase format
                                    ('org (riporta-gg--org-chunk-date s))
                                    ('ledger (riporta-gg--ledger-chunk-date s)))
                                  s))))
      (cl-assert (equal (mapconcat #'cdr chunks "") text) nil
                 "riporta-gg: splitting is not lossless")
      chunks)))


;;; Planning a move

(defun riporta-gg--file-string (file)
  "Return the contents of FILE as a string, or \"\" if it does not exist."
  (if (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (buffer-string))
    ""))

(defun riporta-gg--movable-p (date policy now)
  "Return non-nil if an entry dated DATE is moved under POLICY at NOW."
  (and date
       (pcase policy
         ('tutto t)
         ('mesi-conclusi
          (string< (substring date 0 7) (format-time-string "%Y-%m" now))))))

(defun riporta-gg--dest (dest date)
  "Expand DEST for an entry dated DATE, relative to the archive."
  (expand-file-name (string-replace "%Y" (substring date 0 4) dest)
                    riporta-gg-base-dir))

(cl-defstruct (riporta-gg-move (:constructor riporta-gg-move-create))
  "Entries of one stream that leave the phone file."
  name io-file format io-text keep moved)

(defun riporta-gg--plan-stream (stream now)
  "Return a `riporta-gg-move' for STREAM at time NOW."
  (pcase-let* ((`(,name ,file ,dest ,format ,policy) stream)
               (io-file (expand-file-name file riporta-gg-io-dir))
               (text (riporta-gg--file-string io-file))
               (keep nil) (moved nil))
    (dolist (chunk (riporta-gg--split text format))
      (if (riporta-gg--movable-p (car chunk) policy now)
          (push (cons (riporta-gg--dest dest (car chunk)) (cdr chunk)) moved)
        (push (cdr chunk) keep)))
    (riporta-gg-move-create :name name :io-file io-file :format format
                            :io-text text
                            :keep (apply #'concat (nreverse keep))
                            :moved (nreverse moved))))

(defun riporta-gg--group-by-dest (moved)
  "Group MOVED, a list of (DEST . STRING), as an alist (DEST . STRINGS)."
  (let (groups)
    (dolist (m moved)
      (let ((cell (assoc (car m) groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list (cdr m))))
          (push (list (car m) (cdr m)) groups))))
    (nreverse groups)))


;;; Applying a move

(defun riporta-gg--trim-blank-lines (s)
  "Return S without its final newline and trailing blank lines.
Trailing spaces on the last line with text are kept."
  (replace-regexp-in-string "\\(?:\n[ \t]*\\)+\\'" "" s))

(defun riporta-gg--append-text (existing chunks format)
  "Return the text to append to EXISTING so that it ends with CHUNKS.
Each chunk loses its trailing blank lines; ledger chunks are separated
by an empty line."
  (let* ((sep (pcase format ('org "") ('ledger "\n")))
         (body (mapconcat (lambda (c) (concat (riporta-gg--trim-blank-lines c) "\n"))
                          chunks sep))
         (glue (cond ((string-empty-p existing)
                      (if (eq format 'ledger) "; -*- ledger -*-\n\n" ""))
                     ((not (string-suffix-p "\n" existing))
                      (concat "\n" sep))
                     ((and (eq format 'ledger)
                           (not (string-suffix-p "\n\n" existing)))
                      "\n")
                     (t ""))))
    (concat glue body)))

(defun riporta-gg--write (file text)
  "Write TEXT to FILE as UTF-8, creating its directory if needed."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun riporta-gg--check-buffer (file)
  "Signal an error if a buffer visits FILE with unsaved changes."
  (let ((buf (find-buffer-visiting file)))
    (when (and buf (buffer-modified-p buf))
      (user-error "riporta-gg: %s has unsaved changes" file))))

(defun riporta-gg--revert (file)
  "Revert the buffer visiting FILE, if any."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

(defun riporta-gg--apply-move (move)
  "Append the entries of MOVE to the archive, then shorten the phone file.
Return the list of archive files written."
  (let ((io-file (riporta-gg-move-io-file move))
        (format (riporta-gg-move-format move))
        written)
    (dolist (group (riporta-gg--group-by-dest (riporta-gg-move-moved move)))
      (let* ((dest (car group))
             (old (riporta-gg--file-string dest))
             (add (riporta-gg--append-text old (cdr group) format)))
        (riporta-gg--check-buffer dest)
        (riporta-gg--write dest (concat old add))
        (unless (equal (riporta-gg--file-string dest) (concat old add))
          (error "riporta-gg: verification of %s failed, %s left untouched"
                 dest io-file))
        (riporta-gg--revert dest)
        (push dest written)))
    ;; The phone may have changed the file since it was read: in that
    ;; case keep everything and let the next run sort out the duplicate.
    (unless (equal (riporta-gg--file-string io-file)
                   (riporta-gg-move-io-text move))
      (error "riporta-gg: %s changed while running; entries now appear in both places"
             io-file))
    (riporta-gg--write io-file (riporta-gg-move-keep move))
    (riporta-gg--revert io-file)
    (nreverse written)))


;;; Archiving DONE items

(defun riporta-gg--year-heading-p ()
  "Non-nil if the heading at point is a top-level year.
That is \"* 2025\", or \"* done25\" for the things done that year."
  (and (= (org-outline-level) 1)
       (not (org-get-todo-state))
       (string-match-p "\\`\\(?:[0-9]\\{4\\}\\|done[0-9]\\{2\\}\\)\\'"
                       (org-get-heading t t t t))))

(defun riporta-gg--subtree-done-p ()
  "Non-nil if no heading below the one at point is still to do.
Headings without a todo keyword are notes and do not count."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (done t))
      (while (and done (outline-next-heading) (< (point) end))
        (when (org-entry-is-todo-p)
          (setq done nil)))
      done)))

(defun riporta-gg--whole-task-p ()
  "Non-nil if the heading at point is a whole task, not a part of one.
That is a top-level heading, or a child of a top-level year heading."
  (pcase (org-outline-level)
    (1 t)
    (2 (save-excursion
         (and (org-up-heading-safe)
              (riporta-gg--year-heading-p))))))

(defun riporta-gg--closed-items (file now)
  "Return the DONE items of FILE closed long enough before NOW.
That is at least `riporta-gg-archive-age' days.  Only whole tasks are
archived: top-level DONE headings, or DONE children of a top-level year
heading, with nothing still to do below them.  Each item is
\(POSITION . CLOSED-TIME), last position first, so that archiving
them one after the other does not shift the positions still to visit."
  (let ((limit (time-subtract now (days-to-time riporta-gg-archive-age)))
        items)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (when-let* ((closed (org-entry-get nil "CLOSED"))
                      (time (org-time-string-to-time closed))
                      ((time-less-p time limit))
                      ((riporta-gg--whole-task-p))
                      ((riporta-gg--subtree-done-p)))
            (push (cons (point) time) items)))
        "/DONE" 'file)))
    (sort items (lambda (a b) (> (car a) (car b))))))

(defun riporta-gg--archive-file (file closed)
  "Return the archive file for an item of FILE closed at CLOSED, a time."
  (expand-file-name
   (string-replace "%Y" (format-time-string "%Y" closed)
                   (string-replace "%s" (file-name-base file)
                                   riporta-gg-archive-dest))
   riporta-gg-base-dir))

(defun riporta-gg--item-headings (file items)
  "Return the headings of ITEMS of FILE, in the order of the file.
ITEMS are as returned by `riporta-gg--closed-items'.  Each element is
\(HEADING . ARCHIVE-FILE)."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (mapcar (lambda (item)
               (goto-char (car item))
               (cons (org-get-heading t t t t)
                     (riporta-gg--archive-file file (cdr item))))
             (reverse items)))))

(defun riporta-gg--archive-items (file items)
  "Archive ITEMS of FILE, as returned by `riporta-gg--closed-items'.
Each item goes at the top level of the archive file for the year it
was closed.  Return the list of archive files written.  No backup files
are made: next to FILE they would end up on the phone, and the archive
is in git."
  (let ((make-backup-files nil)
        written)
    (with-current-buffer (find-file-noselect file)
      (dolist (item items)
        (let* ((dest (riporta-gg--archive-file file (cdr item)))
               (org-archive-location (concat dest "::")))
          (make-directory (file-name-directory dest) t)
          (goto-char (car item))
          (org-archive-subtree)
          (with-current-buffer (find-file-noselect dest)
            (save-buffer))
          (cl-pushnew dest written :test #'equal)))
      (save-buffer))
    written))


;;; Mirroring the live lists

(defun riporta-gg--stale-mirrors ()
  "Return the files of `riporta-gg-mirror-files' whose copy is out of date."
  (cl-remove-if
   (lambda (f)
     (let ((src (expand-file-name f riporta-gg-io-dir)))
       (or (not (file-exists-p src))
           (equal (riporta-gg--file-string src)
                  (riporta-gg--file-string
                   (expand-file-name f riporta-gg-base-dir))))))
   riporta-gg-mirror-files))

(defun riporta-gg--mirror ()
  "Copy the stale files of `riporta-gg-mirror-files' to the archive.
Each copy is made read-only.  Return the list of copies written."
  (mapcar
   (lambda (f)
     (let ((dest (expand-file-name f riporta-gg-base-dir)))
       (riporta-gg--check-buffer dest)
       (when (file-exists-p dest)
         (set-file-modes dest #o644))
       (copy-file (expand-file-name f riporta-gg-io-dir) dest t)
       (set-file-modes dest #o444)
       (riporta-gg--revert dest)
       dest))
   (riporta-gg--stale-mirrors)))


;;;###autoload
(defun riporta-gg-protect-copies ()
  "Make a buffer visiting a copy of `riporta-gg-mirror-files' read-only.
The copies in `riporta-gg-base-dir' are overwritten at every run, and
git checkouts make the files writable again; meant for `find-file-hook'."
  (when-let* ((file (buffer-file-name))
              ((member (file-name-nondirectory file) riporta-gg-mirror-files))
              ((file-equal-p (file-name-directory file)
                             (file-name-as-directory
                              (expand-file-name riporta-gg-base-dir)))))
    (setq buffer-read-only t)
    (message "Copia in sola lettura: il file da modificare è %s"
             (abbreviate-file-name
              (expand-file-name (file-name-nondirectory file) riporta-gg-io-dir)))))


;;; Syncthing

(defun riporta-gg--syncthing-api-key ()
  "Return the API key from `riporta-gg-syncthing-config'."
  (let ((xml (riporta-gg--file-string
              (expand-file-name riporta-gg-syncthing-config))))
    (if (string-match "<apikey>\\([^<]+\\)</apikey>" xml)
        (match-string 1 xml)
      (user-error "riporta-gg: no API key in %s" riporta-gg-syncthing-config))))

(defun riporta-gg--syncthing-get (path &optional method)
  "Call the Syncthing REST API at PATH with METHOD (default GET).
Return the parsed JSON, with objects as alists, or signal an error."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers
          `(("X-API-Key" . ,(riporta-gg--syncthing-api-key))))
         (url-show-status nil)
         (buf (url-retrieve-synchronously
               (concat riporta-gg-syncthing-url path) t t 5)))
    (unless buf
      (error "Syncthing does not answer at %s" riporta-gg-syncthing-url))
    (unwind-protect
        (with-current-buffer buf
          (unless (eq url-http-response-status 200)
            (error "Syncthing answered %s to %s" url-http-response-status path))
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (if (eobp)
              nil
            (json-parse-buffer :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil)))
      (kill-buffer buf))))

(defun riporta-gg--syncthing-ids ()
  "Return (DEVICE-ID . FOLDER-ID) for the phone and the io folder."
  (let ((device (cl-find riporta-gg-syncthing-device-name
                         (riporta-gg--syncthing-get "/rest/config/devices")
                         :key (lambda (d) (alist-get 'name d)) :test #'equal))
        (folder (cl-find-if
                 (lambda (f)
                   (if riporta-gg-syncthing-folder-label
                       (equal (alist-get 'label f) riporta-gg-syncthing-folder-label)
                     (string-prefix-p
                      (file-name-as-directory
                       (expand-file-name (alist-get 'path f)))
                      (file-name-as-directory
                       (expand-file-name riporta-gg-io-dir)))))
                 (riporta-gg--syncthing-get "/rest/config/folders"))))
    (unless riporta-gg-syncthing-device-name
      (error "riporta-gg-syncthing-device-name is not set"))
    (unless device
      (error "No Syncthing device named %s" riporta-gg-syncthing-device-name))
    (unless folder
      (error "No Syncthing folder for %s"
             (or riporta-gg-syncthing-folder-label riporta-gg-io-dir)))
    (cons (alist-get 'deviceID device) (alist-get 'id folder))))

(defun riporta-gg--syncthing-checks (now)
  "Ask Syncthing whether the phone and the io folder are in sync at NOW.
Return a list of (OK . DESCRIPTION).  If the API cannot be reached the
list holds a single failed check."
  (condition-case err
      (pcase-let* ((`(,device . ,folder) (riporta-gg--syncthing-ids))
                   (conn (alist-get (intern device)
                                    (alist-get 'connections
                                               (riporta-gg--syncthing-get
                                                "/rest/system/connections"))))
                   (seen (alist-get 'lastSeen
                                    (alist-get (intern device)
                                               (riporta-gg--syncthing-get
                                                "/rest/stats/device"))))
                   (age (and seen (/ (float-time
                                      (time-subtract
                                       now (parse-iso8601-time-string seen)))
                                     60)))
                   (status (riporta-gg--syncthing-get
                            (format "/rest/db/status?folder=%s"
                                    (url-hexify-string folder))))
                   (completion (riporta-gg--syncthing-get
                                (format "/rest/db/completion?folder=%s&device=%s"
                                        (url-hexify-string folder)
                                        (url-hexify-string device))))
                   (last-file (alist-get 'lastFile
                                         (alist-get (intern folder)
                                                    (riporta-gg--syncthing-get
                                                     "/rest/stats/folder")))))
        (list
         (cons (or (alist-get 'connected conn)
                   (and age (< age riporta-gg-syncthing-max-age)))
               (if (alist-get 'connected conn)
                   (format "%s connesso adesso" riporta-gg-syncthing-device-name)
                 (format "%s non connesso, visto l'ultima volta %s (%s min fa, limite %d)"
                         riporta-gg-syncthing-device-name
                         (or seen "mai") (if age (round age) "?")
                         riporta-gg-syncthing-max-age)))
         (cons (and (equal (alist-get 'state status) "idle")
                    (eql (alist-get 'needTotalItems status) 0))
               (format "PC: cartella %s, %s file da ricevere"
                       (alist-get 'state status)
                       (alist-get 'needTotalItems status)))
         (cons (and (>= (alist-get 'completion completion) 100)
                    (eql (alist-get 'needItems completion) 0))
               (format "telefono: completo al %s%%, %s file da ricevere"
                       (alist-get 'completion completion)
                       (alist-get 'needItems completion)))
         (cons t (format "ultimo file ricevuto: %s alle %s"
                         (alist-get 'filename last-file)
                         (alist-get 'at last-file)))))
    (error (list (cons nil (format "Syncthing: %s" (error-message-string err)))))))

(defun riporta-gg--syncthing-sequence ()
  "Return the local sequence number of the io folder, or nil."
  (ignore-errors
    (alist-get 'sequence
               (riporta-gg--syncthing-get
                (format "/rest/db/status?folder=%s"
                        (url-hexify-string (cdr (riporta-gg--syncthing-ids))))))))

(defun riporta-gg--syncthing-push (before)
  "Rescan the io folder and wait for the phone to receive the changes.
BEFORE is the folder sequence read before writing.  Return non-nil if
the phone is in sync within `riporta-gg-syncthing-wait'."
  (condition-case nil
      (pcase-let* ((`(,device . ,folder) (riporta-gg--syncthing-ids))
                   (hex (url-hexify-string folder))
                   (deadline (+ (float-time) riporta-gg-syncthing-wait))
                   (done nil))
        (riporta-gg--syncthing-get (format "/rest/db/scan?folder=%s" hex) "POST")
        (while (and (not done) (< (float-time) deadline))
          (let ((status (riporta-gg--syncthing-get
                         (format "/rest/db/status?folder=%s" hex)))
                (completion (riporta-gg--syncthing-get
                             (format "/rest/db/completion?folder=%s&device=%s"
                                     hex (url-hexify-string device)))))
            (setq done (and (> (alist-get 'sequence status) before)
                            (equal (alist-get 'state status) "idle")
                            (>= (alist-get 'completion completion) 100)))
            (unless done
              (message "riporta-gg: attendo che %s riceva i file (%d%%)..."
                       riporta-gg-syncthing-device-name
                       (alist-get 'completion completion))
              (sleep-for 1))))
        done)
    (error nil)))


;;; Commands

(defun riporta-gg--conflicts ()
  "Return the Syncthing conflict files in `riporta-gg-io-dir'."
  (directory-files riporta-gg-io-dir nil "\\.sync-conflict-"))

(defun riporta-gg--io-files ()
  "Return the absolute names of every phone file `riporta-gg' may touch."
  (mapcar (lambda (f) (expand-file-name f riporta-gg-io-dir))
          (append (mapcar #'cadr riporta-gg-streams)
                  riporta-gg-archive-files)))

(defun riporta-gg--preflight (force now)
  "Save the phone buffers and check Syncthing at NOW.
Return the list of checks.  Signal an error when a check fails, unless
FORCE is non-nil and the user confirms."
  (let ((conflicts (riporta-gg--conflicts)))
    (when conflicts
      (user-error "riporta-gg: conflitti Syncthing in %s: %s"
                  riporta-gg-io-dir (string-join conflicts ", "))))
  (dolist (file (riporta-gg--io-files))
    (let ((buf (find-buffer-visiting file)))
      (when (and buf (buffer-modified-p buf))
        (with-current-buffer buf (save-buffer)))))
  (let* ((checks (riporta-gg--syncthing-checks now))
         (failed (cl-remove-if #'car checks)))
    (when failed
      (unless (and force
                   (yes-or-no-p
                    (format "Controlli Syncthing falliti (%s). Forzare comunque? "
                            (mapconcat #'cdr failed "; "))))
        (user-error "riporta-gg: %s" (mapconcat #'cdr failed "; "))))
    checks))

(defun riporta-gg--git-commit (files message)
  "Commit FILES in `riporta-gg-base-dir' with MESSAGE.
Return nil on success, or git's output on failure."
  (let ((default-directory (expand-file-name riporta-gg-base-dir)))
    (with-temp-buffer
      (if (and (= 0 (apply #'call-process "git" nil t nil "add" "--"
                           (mapcar #'file-relative-name files)))
               (= 0 (call-process "git" nil t nil "commit" "-m" message)))
          nil
        (string-trim (buffer-string))))))

(defun riporta-gg--preview (checks moves archives mirrors)
  "Show CHECKS, MOVES, ARCHIVES and MIRRORS in the *riporta* buffer."
  (with-current-buffer (get-buffer-create "*riporta*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Syncthing\n")
      (dolist (c checks)
        (insert (format "  [%s] %s\n" (if (car c) "ok" "NO") (cdr c))))
      (insert "\nFlussi\n")
      (dolist (m moves)
        (insert (format "  %s: %d voci da spostare\n"
                        (riporta-gg-move-name m)
                        (length (riporta-gg-move-moved m))))
        (dolist (g (riporta-gg--group-by-dest (riporta-gg-move-moved m)))
          (insert (format "    -> %s  %d  (%s ... %s)\n"
                          (file-relative-name (car g) riporta-gg-base-dir)
                          (length (cdr g))
                          (riporta-gg--first-line (cadr g))
                          (riporta-gg--first-line (car (last g)))))))
      (insert "\nDONE da archiviare\n")
      (dolist (a archives)
        (insert (format "  %s: %d voci\n" (file-name-nondirectory (car a))
                        (length (cdr a))))
        (pcase-dolist (`(,heading . ,dest)
                       (riporta-gg--item-headings (car a) (cdr a)))
          (insert (format "    %s  -> %s\n"
                          (riporta-gg--first-line heading)
                          (file-relative-name dest riporta-gg-base-dir)))))
      (insert "\nCopie da aggiornare\n")
      (dolist (f mirrors)
        (insert (format "  %s\n" f)))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun riporta-gg--first-line (s)
  "Return the first line of S, shortened."
  (truncate-string-to-width (car (split-string s "\n")) 50 nil nil "…"))

;;;###autoload
(defun riporta-gg (&optional force)
  "Move the ready entries from the phone files to the archive and commit.
Streams are described by `riporta-gg-streams'; DONE items closed long
enough ago in `riporta-gg-archive-files' are archived too, and the
files of `riporta-gg-mirror-files' are copied to the archive.  Nothing
happens unless Syncthing reports the phone as recently seen and the
folder in sync; with a prefix argument FORCE, offer to go on anyway."
  (interactive "P")
  (let* ((now (current-time))
         (checks (riporta-gg--preflight force now))
         (moves (cl-remove-if-not #'riporta-gg-move-moved
                                  (mapcar (lambda (s) (riporta-gg--plan-stream s now))
                                          riporta-gg-streams)))
         (archives (cl-loop for f in riporta-gg-archive-files
                            for file = (expand-file-name f riporta-gg-io-dir)
                            for items = (and (file-exists-p file)
                                             (riporta-gg--closed-items file now))
                            when items collect (cons file items)))
         ;; Archiving changes the lists, so their copies will be stale too.
         (mirrors (delete-dups
                   (append (riporta-gg--stale-mirrors)
                           (cl-intersection
                            (mapcar (lambda (a) (file-name-nondirectory (car a)))
                                    archives)
                            riporta-gg-mirror-files :test #'equal)))))
    (riporta-gg--preview checks moves archives mirrors)
    (if (not (or moves archives mirrors))
        (message "riporta-gg: niente da spostare")
      (when (y-or-n-p "Spostare queste voci nell'archivio? ")
        (let* ((sequence (riporta-gg--syncthing-sequence))
               (written (append
                        (mapcan #'riporta-gg--apply-move moves)
                        (mapcan (lambda (a) (riporta-gg--archive-items (car a) (cdr a)))
                                archives)
                        (riporta-gg--mirror))))
          (let ((err (riporta-gg--git-commit
                      (delete-dups written)
                      (format-time-string "riporta %F" now))))
            (when err
              (display-warning 'riporta-gg (format "git commit fallito:\n%s" err))))
          (cond
           ((not (or moves archives))
            (message "riporta-gg: copie aggiornate"))
           ((and sequence (riporta-gg--syncthing-push sequence))
            (message "riporta-gg: fatto, %s è allineato"
                     riporta-gg-syncthing-device-name))
           (t
            (display-warning
             'riporta-gg
             (format "%s non ha ancora ricevuto i file accorciati: non modificarli sul telefono finché non sincronizza"
                     riporta-gg-syncthing-device-name)))))))))

;;;###autoload
(defun riporta-gg-archive-done (&optional force)
  "Archive the DONE items of `riporta-gg-archive-files' and commit.
Only items closed at least `riporta-gg-archive-age' days ago are moved.
FORCE is handled as in `riporta-gg'."
  (interactive "P")
  (let ((riporta-gg-streams nil))
    (riporta-gg force)))

(provide 'riporta-gg)
;;; riporta-gg.el ends here
