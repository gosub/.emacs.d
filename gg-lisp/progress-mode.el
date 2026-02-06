;;; progress-mode.el --- Visual time progress display -*- lexical-binding: t; -*-

(defgroup progress-mode nil
  "Visual time progress display."
  :group 'applications)

(defcustom progress-dot "●"
  "Character used to display progress units."
  :type 'string
  :group 'progress-mode)

(defcustom progress-day-columns 30
  "Number of columns in the day progress display."
  :type 'integer
  :group 'progress-mode)

(defcustom progress-week-columns 24
  "Number of columns in the week progress display."
  :type 'integer
  :group 'progress-mode)

(defcustom progress-year-columns 30
  "Number of columns in the year progress display."
  :type 'integer
  :group 'progress-mode)

(defcustom progress-week-start 'monday
  "First day of the week."
  :type '(choice (const :tag "Monday" monday)
                 (const :tag "Sunday" sunday))
  :group 'progress-mode)

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
  (dotimes (i total)
      (let ((face (if (< i elapsed)
                     'progress-done-dot
                     'progress-remaining-dot)))
        (insert (propertize progress-dot 'face face))
        (insert " ")
        (when (and (= (mod i cols) (1- cols)) (< i (1- total)))
          (insert "\n")))))

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

(defun progress--day-info ()
  "Return plist with :elapsed, :total, :hour, :minute for the current day."
  (let* ((now (decode-time (current-time)))
         (hour (nth 2 now))
         (minute (nth 1 now)))
    (list :elapsed (+ (* hour 60) minute)
          :total (* 24 60)
          :hour hour
          :minute minute)))

(defun progress--week-info ()
  "Return plist with :elapsed, :total, :week-dow for the current week.
Respects `progress-week-start'."
  (let* ((now (decode-time (current-time)))
         (hour (nth 2 now))
         (dow (nth 6 now))
         (offset (if (eq progress-week-start 'monday) 6 0))
         (week-dow (mod (+ dow offset) 7)))
    (list :elapsed (+ (* week-dow 24) hour)
          :total (* 7 24)
          :week-dow (1+ week-dow))))

(defun progress--year-info ()
  "Return plist with :elapsed, :total, :year, :leap for the current year."
  (let* ((now (decode-time (current-time)))
         (year (nth 5 now))
         (is-leap (date-leap-year-p year))
         (days-in-year (if is-leap 366 365))
         (day-of-year (1+ (time-to-day-in-year (current-time)))))
    (list :elapsed day-of-year
          :total days-in-year
          :year year
          :leap is-leap)))

(defun progress--display (info-fn buffer-name title-fn unit cols refresh-fn)
  "Display a progress buffer.
INFO-FN returns a plist with :elapsed and :total.
BUFFER-NAME is the buffer to create.
TITLE-FN is called with the info plist to produce the title string.
UNIT is the label for the counted units.
COLS is the number of columns.
REFRESH-FN is stored for the g keybinding."
  (let* ((info (funcall info-fn))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (progress--insert-section
         (funcall title-fn info)
         (plist-get info :elapsed) (plist-get info :total) unit
         cols (/ (plist-get info :total) cols)))
      (unless (derived-mode-p 'progress-mode)
        (progress-mode))
      (setq progress--refresh-function refresh-fn)
      (goto-char (point-min))
      (display-buffer buffer))))

(defun progress-day ()
  "Display progress for the current day in minutes."
  (interactive)
  (progress--display
   #'progress--day-info "*Day Progress*"
   (lambda (info) (format "Day (%02d:%02d)" (plist-get info :hour) (plist-get info :minute)))
   "Minutes" progress-day-columns #'progress-day))

(defun progress-week ()
  "Display progress for the current week in hours."
  (interactive)
  (progress--display
   #'progress--week-info "*Week Progress*"
   (lambda (info) (format "Week (day %d/7)" (plist-get info :week-dow)))
   "Hours" progress-week-columns #'progress-week))

(defun progress-year ()
  "Display progress for the current year in days."
  (interactive)
  (progress--display
   #'progress--year-info "*Year Progress*"
   (lambda (info) (format "Year %d%s" (plist-get info :year) (if (plist-get info :leap) " (leap)" "")))
   "Days" progress-year-columns #'progress-year))

(provide 'progress-mode)
;;; progress-mode.el ends here
