;;; watch.el --- simple "watch" for Emacs  -*- lexical-binding: t; -*-

;; Usage:
;;   M-x watch-start
;;   or programmatically:
;;   (watch-start "date" :interval 1 :differences t :timestamp t)

(defgroup watch nil
  "Periodically run a shell command and display its output."
  :group 'tools)

(defface watch-diff-face
  '((t :inherit highlight))
  "Face for lines that differ from previous run."
  :group 'watch)

(define-derived-mode watch-mode special-mode "Watch"
  "Major mode for `watch.el' output buffers."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'watch--force-refresh)
  (use-local-map (copy-keymap special-mode-map))
  (local-set-key (kbd "g") #'watch-refresh)
  (local-set-key (kbd "q") #'watch-quit))

(cl-defstruct (watch--state (:constructor watch--make-state))
  command interval differences timestamp strip-ansi
  timer running prev-lines overlays process name last-run)

(defvar-local watch--buffer-state nil
  "Internal state for the current watch buffer.")

(defun watch--header-line (st)
  (if (not st)
      ""
    (let* ((base (format "watch: every %.1fs: %s"
                         (watch--state-interval st)
                         (watch--state-command st)))
           (timestamp (and (watch--state-timestamp st)
                           (watch--state-last-run st))))
      (if (not timestamp)
          base
        (let* ((gap (propertize
                     " "
                     'display `(space :align-to (- right ,(+ 1 (length timestamp)))))))
          (list base gap timestamp))))))

(defun watch--buffer-name (name command)
  (format "*watch:%s* %s" (or name "") command))

(defun watch--kill (st)
  (when (watch--state-timer st)
    (cancel-timer (watch--state-timer st)))
  (when-let ((p (watch--state-process st)))
    (when (process-live-p p) (kill-process p))))

(defun watch--clear-overlays (st)
  (when (watch--state-overlays st)
    (mapc #'delete-overlay (watch--state-overlays st))
    (setf (watch--state-overlays st) nil)))

(defun watch--highlight-differences (st lines)
  (let ((prev (or (watch--state-prev-lines st) [])))
    (watch--clear-overlays st)
    (cl-loop for i from 0 below (max (length lines) (length prev)) do
             (let ((new (and (< i (length lines)) (aref lines i)))
                   (old (and (< i (length prev))  (aref prev i))))
               (unless (equal new old)
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line i)
                   (let* ((beg (line-beginning-position))
                          (end (line-end-position))
                          (ov  (make-overlay beg end)))
                     (overlay-put ov 'face 'watch-diff-face)
                     (push ov (watch--state-overlays st)))))))))

(defun watch--string-lines (s)
  (vconcat (split-string (string-remove-suffix "\n" s) "\n")))

(defun watch--insert-output (st out)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when (watch--state-timestamp st)
      (setf (watch--state-last-run st)
            (format-time-string "%Y-%m-%d %H:%M:%S")))
    (when (watch--state-strip-ansi st)
      (setq out (ansi-color-apply out)))
    (insert out)
    (goto-char (point-min))
    (when (eobp) (insert "\n")))
  (force-mode-line-update))

(defun watch--run (st buf)
  ;; skip if previous run still active
  (when (not (watch--state-running st))
    (setf (watch--state-running st) t)
    (let* ((cmd (watch--state-command st))
           (proc (make-process
                  :name (format "watch:%s" (watch--state-name st))
                  :buffer (generate-new-buffer " *watch-temp*")
                  :command (list shell-file-name shell-command-switch cmd)
                  :noquery t
                  :connection-type 'pipe
                  :sentinel
                  (lambda (p _msg)
                    (when (memq (process-status p) '(exit signal))
                      (unwind-protect
                          (with-current-buffer buf
                            (let ((out (with-current-buffer (process-buffer p)
                                         (buffer-string))))
                              (watch--insert-output st out)
                              (let ((lines (watch--string-lines out)))
                                (when (watch--state-differences st)
                                  (watch--highlight-differences st lines))
                                (setf (watch--state-prev-lines st) lines))))
                        (kill-buffer (process-buffer p))
                        (setf (watch--state-running st) nil)))))))
      (setf (watch--state-process st) proc))))

(defun watch-refresh ()
  "Run the watched command immediately."
  (interactive)
  (let ((st watch--buffer-state))
    (if (not st)
        (user-error "Not a watch buffer")
      (watch--run st (current-buffer)))))

(defun watch--force-refresh (&rest _)
  (watch-refresh))

(defun watch-quit ()
  "Quit the watch buffer and cancel timers."
  (interactive)
  (let ((st watch--buffer-state))
    (when st (watch--kill st)))
  (setq watch--buffer-state nil)
  (quit-window t))

(cl-defun watch-start (command &key (interval 2.0) differences timestamp (strip-ansi t) name)
  "Start watching COMMAND every INTERVAL seconds in a new buffer.

Options:
  :differences  — highlight changed lines (like `watch -d`)
  :timestamp    — show a timestamp in the header line
  :strip-ansi   — render ANSI colors (default t)
  :name         — label to distinguish multiple watches"
  (interactive
   (list (read-shell-command "watch command: ")
         :interval (read-number "Interval (seconds): " 2)
         :differences (y-or-n-p "Highlight differences? ")
         :timestamp (y-or-n-p "Show timestamp? ")))
  (let* ((bufname (watch--buffer-name name command))
         (buf (get-buffer-create bufname))
         (st (watch--make-state :command command :interval interval
                                :differences differences :timestamp timestamp
                                :strip-ansi strip-ansi :name (or name command))))
    (with-current-buffer buf
      (when watch--buffer-state
        (watch--kill watch--buffer-state))
      (watch-mode)
      (setq watch--buffer-state st)
      (setq-local header-line-format
                  '((:eval (watch--header-line watch--buffer-state))))
      ;; make state accessible & cleaned up
      (setq-local buffer-quit-function #'watch-quit)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Starting watch for: %s\n" command)))
      (add-hook 'kill-buffer-hook (lambda () (watch--kill st)) nil t))
    ;; initial run and schedule
    (with-current-buffer buf
      (watch--run st buf)
      (setf (watch--state-timer st)
            (run-at-time (watch--state-interval st)
                         (watch--state-interval st)
                         (lambda () (when (buffer-live-p buf)
                                      (with-current-buffer buf
                                        (watch--run st buf)))))))
    (pop-to-buffer buf)
    buf))

(provide 'watch)
;;; watch.el ends here
