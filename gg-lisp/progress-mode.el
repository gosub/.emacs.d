;;; progress-mode.el --- Visual time progress display -*- lexical-binding: t; -*-

(defgroup progress-mode nil
  "Visual time progress display."
  :group 'applications)

(defface progress-done-dot
  '((t :inherit bold))
  "Face for elapsed units."
  :group 'progress-mode)

(defface progress-remaining-dot
  '((t :inherit shadow))
  "Face for remaining units."
  :group 'progress-mode)

(defvar progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'progress-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "k" #'kill-current-buffer)
    map)
  "Keymap for `progress-mode'.")

(defvar-local progress--refresh-function nil
  "Function to call to refresh the current progress buffer.")

(define-derived-mode progress-mode special-mode "Progress"
  "Major mode for displaying time progress."
  :group 'progress-mode)

(defun progress-refresh ()
  "Refresh the current progress buffer."
  (interactive)
  (when progress--refresh-function
    (funcall progress--refresh-function)))

(defun progress--insert-dots (elapsed total cols)
  "Insert TOTAL dots, first ELAPSED with `progress-done-dot' face.
COLS is the number of dots per row."
  (let ((dot "●"))
    (dotimes (i total)
      (let ((face (if (< i elapsed)
                     'progress-done-dot
                     'progress-remaining-dot)))
        (insert (propertize dot 'face face))
        (insert " ")
        (when (and (= (mod i cols) (1- cols)) (< i (1- total)))
          (insert "\n"))))))

(defun progress--insert-section (title elapsed total unit cols rows)
  "Insert a progress section with TITLE.
ELAPSED and TOTAL are counts of UNIT (a string).
COLS is the number of dots per row, ROWS is the number of rows.
The displayed total is (* COLS ROWS)."
  (let ((display-total (* cols rows)))
    (insert (format "%s (%d %s total):\n\n" title display-total unit))
    (progress--insert-dots elapsed display-total cols)
    (insert "\n\n")
    (insert (format "%s passed: %d\n" unit elapsed))
    (insert (format "%s remaining: %d\n" unit (- display-total elapsed)))
    (insert (format "Progress: %.1f%%\n"
                    (* 100.0 (/ (float elapsed) display-total))))))

(defun progress--day-elapsed ()
  "Return (ELAPSED . TOTAL) minutes for the current day."
  (let* ((now (decode-time (current-time)))
         (hour (nth 2 now))
         (minute (nth 1 now)))
    (cons (+ (* hour 60) minute) (* 24 60))))

(defun progress--week-elapsed ()
  "Return (ELAPSED . TOTAL) hours for the current week (Monday start)."
  (let* ((now (decode-time (current-time)))
         (hour (nth 2 now))
         (dow (nth 6 now))
         (week-dow (mod (+ dow 6) 7)))
    (cons (+ (* week-dow 24) hour) (* 7 24))))

(defun progress--year-elapsed ()
  "Return (ELAPSED . TOTAL) days for the current year."
  (let* ((now (decode-time (current-time)))
         (year (nth 5 now))
         (is-leap (date-leap-year-p year))
         (days-in-year (if is-leap 366 365))
         (day-of-year (1+ (time-to-day-in-year (current-time)))))
    (cons day-of-year days-in-year)))

(defun display-day-progress ()
  "Display progress for the current day in minutes."
  (interactive)
  (let* ((now (decode-time (current-time)))
         (hour (nth 2 now))
         (minute (nth 1 now))
         (day (progress--day-elapsed))
         (buffer (get-buffer-create "*Day Progress*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (progress--insert-section
         (format "Day (%02d:%02d)" hour minute)
         (car day) (cdr day) "Minutes" 60 24))
      (unless (derived-mode-p 'progress-mode)
        (progress-mode))
      (setq progress--refresh-function #'display-day-progress)
      (goto-char (point-min))
      (display-buffer buffer))))

(defun display-week-progress ()
  "Display progress for the current week in hours."
  (interactive)
  (let* ((now (decode-time (current-time)))
         (dow (nth 6 now))
         (week-dow (1+ (mod (+ dow 6) 7)))
         (week (progress--week-elapsed))
         (buffer (get-buffer-create "*Week Progress*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (progress--insert-section
         (format "Week (day %d/7)" week-dow)
         (car week) (cdr week) "Hours" 24 7))
      (unless (derived-mode-p 'progress-mode)
        (progress-mode))
      (setq progress--refresh-function #'display-week-progress)
      (goto-char (point-min))
      (display-buffer buffer))))

(defun display-year-progress ()
  "Display progress for the current year in days."
  (interactive)
  (let* ((now (decode-time (current-time)))
         (year (nth 5 now))
         (is-leap (date-leap-year-p year))
         (yr (progress--year-elapsed))
         (buffer (get-buffer-create "*Year Progress*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (progress--insert-section
         (format "Year %d%s" year (if is-leap " (leap)" ""))
         (car yr) (cdr yr) "Days" 30 13))
      (unless (derived-mode-p 'progress-mode)
        (progress-mode))
      (setq progress--refresh-function #'display-year-progress)
      (goto-char (point-min))
      (display-buffer buffer))))

(provide 'progress-mode)
;;; progress-mode.el ends here
