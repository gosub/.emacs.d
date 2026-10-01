;;; riporta-gg-test.el --- Tests for riporta-gg -*- lexical-binding: t; -*-

(require 'ert)
(require 'riporta-gg)

(defconst rgt-diario
  "* 2026-09-29
** bad: X
** giretto con Mya
* 2026-09-30
** spesa

* 2026-10-01
** oggi
")

(defconst rgt-slurp
  "* [[https://youtube.com/watch?v=2024-01-01x][Video 2020-02-02]] [2026-09-13 Sun] :yt:
* Libro senza data :book:
* [[https://x][Altro]] [2026-10-01 Thu] :yt:
")

(defconst rgt-ledger
  "; -*- ledger -*-

2026-09-14 Bar
    attivo:postepay                           -2.40 EUR
    oneri:bar

; 2026-09-15 annullato
;    attivo:postepay    -1 EUR

2026-10-01 Elite
    attivo:postepay                           -5.84 EUR
    oneri:alimentari
")

(defmacro rgt-with-dirs (&rest body)
  "Run BODY with fresh io and base directories."
  (declare (indent 0))
  `(let* ((root (make-temp-file "rgt" t))
          (riporta-gg-io-dir (file-name-as-directory (expand-file-name "io" root)))
          (riporta-gg-base-dir (file-name-as-directory (expand-file-name "base" root))))
     (make-directory riporta-gg-io-dir t)
     (make-directory riporta-gg-base-dir t)
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun rgt-write (dir file text)
  (riporta-gg--write (expand-file-name file dir) text))

(defun rgt-read (dir file)
  (riporta-gg--file-string (expand-file-name file dir)))

(defconst rgt-oct-1 (encode-time (parse-time-string "2026-10-01 10:00:00")))


;;; Splitting

(ert-deftest rgt-split-org ()
  (let ((chunks (riporta-gg--split rgt-diario 'org)))
    (should (equal (mapcar #'car chunks) '("2026-09-29" "2026-09-30" "2026-10-01")))
    (should (equal (mapconcat #'cdr chunks "") rgt-diario))))

(ert-deftest rgt-split-org-preamble ()
  (let ((chunks (riporta-gg--split (concat "#+title: x\n\n" rgt-diario) 'org)))
    (should (equal (car chunks) '(nil . "#+title: x\n\n")))))

(ert-deftest rgt-split-slurp-uses-last-date ()
  (should (equal (mapcar #'car (riporta-gg--split rgt-slurp 'org))
                 '("2026-09-13" nil "2026-10-01"))))

(ert-deftest rgt-split-ledger ()
  (let ((chunks (riporta-gg--split rgt-ledger 'ledger)))
    (should (equal (mapcar #'car chunks) '(nil "2026-09-14" "2026-09-15" "2026-10-01")))
    (should (equal (mapconcat #'cdr chunks "") rgt-ledger))))

(ert-deftest rgt-split-ledger-directive ()
  (should (equal (mapcar #'car (riporta-gg--split "C 1 BP = 7.23 EUR\n" 'ledger))
                 '(nil))))


;;; Planning and applying

(ert-deftest rgt-plan-mesi-conclusi ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "io.org" rgt-diario)
    (let ((m (riporta-gg--plan-stream '(diario "io.org" "done/%Y.org" org mesi-conclusi)
                                      rgt-oct-1)))
      (should (= 2 (length (riporta-gg-move-moved m))))
      (should (equal (riporta-gg-move-keep m) "* 2026-10-01\n** oggi\n")))))

(ert-deftest rgt-apply-org ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "io.org" rgt-diario)
    ;; Existing archive without a final newline.
    (rgt-write riporta-gg-base-dir "done/2026.org" "* 2026-09-28\n** ieri")
    (riporta-gg--apply-move
     (riporta-gg--plan-stream '(diario "io.org" "done/%Y.org" org mesi-conclusi) rgt-oct-1))
    (should (equal (rgt-read riporta-gg-base-dir "done/2026.org")
                   "* 2026-09-28\n** ieri\n* 2026-09-29\n** bad: X\n** giretto con Mya\n* 2026-09-30\n** spesa\n"))
    (should (equal (rgt-read riporta-gg-io-dir "io.org") "* 2026-10-01\n** oggi\n"))))

(ert-deftest rgt-apply-keeps-trailing-spaces ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "io.org" "* 2026-09-01\n** x \n\n\n")
    (riporta-gg--apply-move
     (riporta-gg--plan-stream '(diario "io.org" "done/%Y.org" org mesi-conclusi) rgt-oct-1))
    (should (equal (rgt-read riporta-gg-base-dir "done/2026.org") "* 2026-09-01\n** x \n"))))

(ert-deftest rgt-apply-ledger-tutto-new-file ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "contab.txt" rgt-ledger)
    (riporta-gg--apply-move
     (riporta-gg--plan-stream '(contab "contab.txt" "contab/%Y.ledger" ledger tutto) rgt-oct-1))
    (should (equal (rgt-read riporta-gg-io-dir "contab.txt") "; -*- ledger -*-\n\n"))
    (should (equal (rgt-read riporta-gg-base-dir "contab/2026.ledger")
                   (concat "; -*- ledger -*-\n\n"
                           "2026-09-14 Bar\n    attivo:postepay                           -2.40 EUR\n    oneri:bar\n\n"
                           "; 2026-09-15 annullato\n;    attivo:postepay    -1 EUR\n\n"
                           "2026-10-01 Elite\n    attivo:postepay                           -5.84 EUR\n    oneri:alimentari\n")))))

(ert-deftest rgt-apply-ledger-existing ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "contab.txt" "2026-09-14 Bar\n    a  1 EUR\n    b\n")
    (rgt-write riporta-gg-base-dir "contab/2026.ledger" "2026-09-01 X\n    a  1 EUR\n    b\n")
    (riporta-gg--apply-move
     (riporta-gg--plan-stream '(contab "contab.txt" "contab/%Y.ledger" ledger tutto) rgt-oct-1))
    (should (equal (rgt-read riporta-gg-base-dir "contab/2026.ledger")
                   "2026-09-01 X\n    a  1 EUR\n    b\n\n2026-09-14 Bar\n    a  1 EUR\n    b\n"))
    (should (equal (rgt-read riporta-gg-io-dir "contab.txt") ""))))

(ert-deftest rgt-apply-splits-years ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "slurp.org" "* a [2025-12-31 Wed]\n* b [2026-01-01 Thu]\n")
    (riporta-gg--apply-move
     (riporta-gg--plan-stream '(slurp "slurp.org" "slurp/%Y.org" org mesi-conclusi) rgt-oct-1))
    (should (equal (rgt-read riporta-gg-base-dir "slurp/2025.org") "* a [2025-12-31 Wed]\n"))
    (should (equal (rgt-read riporta-gg-base-dir "slurp/2026.org") "* b [2026-01-01 Thu]\n"))))

(ert-deftest rgt-apply-aborts-if-io-changed ()
  (rgt-with-dirs
    (rgt-write riporta-gg-io-dir "io.org" rgt-diario)
    (let ((m (riporta-gg--plan-stream '(diario "io.org" "done/%Y.org" org mesi-conclusi)
                                      rgt-oct-1)))
      (rgt-write riporta-gg-io-dir "io.org" (concat rgt-diario "** dal telefono\n"))
      (should-error (riporta-gg--apply-move m))
      ;; The phone edit survives.
      (should (string-suffix-p "** dal telefono\n" (rgt-read riporta-gg-io-dir "io.org"))))))


;;; Archiving DONE

(ert-deftest rgt-archive-done ()
  (rgt-with-dirs
    (let ((riporta-gg-archive-age 7)
          (org-archive-save-context-info '(time olpath)))
      (rgt-write riporta-gg-io-dir "todo.org"
                 "* errands\n** DONE vecchio\nCLOSED: [2026-09-01 Tue 10:00]\n** DONE recente\nCLOSED: [2026-09-30 Wed 10:00]\n** aperto\n")
      (let* ((file (expand-file-name "todo.org" riporta-gg-io-dir))
             (items (riporta-gg--closed-items file rgt-oct-1)))
        (should (= 1 (length items)))
        (riporta-gg--archive-items file items)
        (kill-buffer (find-buffer-visiting file))
        (let ((todo (rgt-read riporta-gg-io-dir "todo.org"))
              (arch (rgt-read riporta-gg-base-dir "archive/todo_2026.org")))
          (should-not (string-match-p "vecchio" todo))
          (should (string-match-p "recente" todo))
          (should (string-match-p "\\* DONE vecchio" arch))
          (should (string-match-p ":ARCHIVE_OLPATH: errands" arch))
          (should-not (directory-files riporta-gg-io-dir nil "~\\'"))
          (should-not (directory-files (expand-file-name "archive" riporta-gg-base-dir)
                                       nil "~\\'")))
        (kill-buffer (find-buffer-visiting
                      (expand-file-name "archive/todo_2026.org" riporta-gg-base-dir)))))))


;;; Syncthing

(defun rgt-fake-api (overrides)
  "Return a fake `riporta-gg--syncthing-get' with OVERRIDES of the defaults."
  (let ((answers
         `(("/rest/config/devices" . (((deviceID . "PHONE") (name . "moto_edge30"))))
           ("/rest/config/folders" . (((id . "fold") (label . "io"))))
           ("/rest/system/connections" . ((connections . ((PHONE . ((connected . t)))))))
           ("/rest/stats/device" . ((PHONE . ((lastSeen . "2026-10-01T09:58:00+02:00")))))
           ("/rest/db/status?folder=fold" . ((state . "idle") (needTotalItems . 0) (sequence . 5)))
           ("/rest/db/completion?folder=fold&device=PHONE" . ((completion . 100) (needItems . 0)))
           ("/rest/stats/folder" . ((fold . ((lastFile . ((filename . "org/io.org") (at . "x"))))))))))
    (lambda (path &optional _method)
      (let ((a (assoc path (append overrides answers))))
        (cond ((null a) (error "unexpected %s" path))
              ((eq (cdr a) 'down) (error "Syncthing does not answer"))
              (t (cdr a)))))))

(defun rgt-checks (overrides)
  (cl-letf (((symbol-function 'riporta-gg--syncthing-get) (rgt-fake-api overrides)))
    (riporta-gg--syncthing-checks rgt-oct-1)))

(defun rgt-all-ok (checks) (cl-every #'car checks))

(ert-deftest rgt-sync-ok ()
  (should (rgt-all-ok (rgt-checks nil))))

(defconst rgt-offline
  (list (cons "/rest/system/connections"
              '((connections . ((PHONE . ((connected . nil)))))))))

(ert-deftest rgt-sync-offline-recent-ok ()
  (should (rgt-all-ok (rgt-checks rgt-offline))))

(ert-deftest rgt-sync-offline-old ()
  (should-not
   (rgt-all-ok
    (rgt-checks
     (cons (cons "/rest/stats/device"
                 '((PHONE . ((lastSeen . "2026-09-30T09:00:00+02:00")))))
           rgt-offline)))))

(ert-deftest rgt-sync-pc-syncing ()
  (should-not
   (rgt-all-ok
    (rgt-checks
     (list (cons "/rest/db/status?folder=fold"
                 '((state . "syncing") (needTotalItems . 1) (sequence . 5))))))))

(ert-deftest rgt-sync-phone-incomplete ()
  (should-not
   (rgt-all-ok
    (rgt-checks
     (list (cons "/rest/db/completion?folder=fold&device=PHONE"
                 '((completion . 80) (needItems . 2))))))))

(ert-deftest rgt-sync-api-down ()
  (let ((checks (rgt-checks '(("/rest/config/devices" . down)))))
    (should (= 1 (length checks)))
    (should-not (caar checks))))

(ert-deftest rgt-preflight-refuses-and-conflicts ()
  (rgt-with-dirs
    (cl-letf (((symbol-function 'riporta-gg--syncthing-get)
               (rgt-fake-api '(("/rest/config/devices" . down)))))
      (should-error (riporta-gg--preflight nil rgt-oct-1) :type 'user-error)
      (rgt-write riporta-gg-io-dir "io.sync-conflict-20260101-ABC.org" "x")
      (should-error (riporta-gg--preflight t rgt-oct-1) :type 'user-error))))


;;; End to end

(ert-deftest rgt-end-to-end ()
  (rgt-with-dirs
    (let ((default-directory riporta-gg-base-dir))
      (call-process "git" nil nil nil "init" "-q")
      (call-process "git" nil nil nil "config" "user.email" "t@t")
      (call-process "git" nil nil nil "config" "user.name" "t"))
    (rgt-write riporta-gg-io-dir "io.org" rgt-diario)
    (rgt-write riporta-gg-io-dir "slurp.org" rgt-slurp)
    (rgt-write riporta-gg-io-dir "contab.txt" rgt-ledger)
    (rgt-write riporta-gg-io-dir "todo.org" "* x\n")
    (rgt-write riporta-gg-io-dir "idee.org" "* y\n")
    (let ((pushed nil))
      (cl-letf (((symbol-function 'riporta-gg--syncthing-get) (rgt-fake-api nil))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'current-time) (lambda () rgt-oct-1))
                ((symbol-function 'riporta-gg--syncthing-push)
                 (lambda (seq) (setq pushed seq) t)))
        (riporta-gg))
      (should (eql pushed 5)))
    (should (equal (rgt-read riporta-gg-io-dir "io.org") "* 2026-10-01\n** oggi\n"))
    (should (equal (rgt-read riporta-gg-io-dir "slurp.org")
                   "* Libro senza data :book:\n* [[https://x][Altro]] [2026-10-01 Thu] :yt:\n"))
    (should (file-exists-p (expand-file-name "contab/2026.ledger" riporta-gg-base-dir)))
    (let ((default-directory riporta-gg-base-dir))
      (should (string-match-p
               "riporta 2026-10-01"
               (shell-command-to-string "git log --oneline")))
      (should (equal "" (shell-command-to-string "git status --porcelain"))))))

;;; riporta-gg-test.el ends here
