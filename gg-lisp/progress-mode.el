;;; progress-mode.el --- Visual year progress display -*- lexical-binding: t; -*-

(defgroup progress-mode nil
  "Visual year progress display."
  :group 'applications)

(defface year-progress-dark-dot
  '((t :inherit bold))
  "Face for days that have already passed in the year."
  :group 'progress-mode)

(defface year-progress-light-dot
  '((t :inherit shadow))
  "Face for days that have not yet passed in the year."
  :group 'progress-mode)

(defvar progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'display-year-progress)
    (define-key map "q" #'quit-window)
    (define-key map "k" #'kill-current-buffer)
    map)
  "Keymap for `progress-mode'.")

(define-derived-mode progress-mode special-mode "Progress"
  "Major mode for displaying year progress."
  :group 'progress-mode)

(defun display-year-progress ()
  "Display a visual representation of passed and remaining days in the current year.
Uses dark dots for passed days and light dots for remaining days."
  (interactive)
  (let* ((current-date (decode-time (current-time)))
         (current-year (nth 5 current-date))
         (is-leap-year (date-leap-year-p current-year))
         (days-in-year (if is-leap-year 366 365))
         (current-day-of-year (1+ (time-to-day-in-year (current-time))))
         (buffer (get-buffer-create "*Year Progress*"))
         (dot "●"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Progress for %d (%d days total):\n\n"
                        current-year days-in-year))
        (dotimes (day days-in-year)
          (let ((face (if (<= (1+ day) current-day-of-year)
                         'year-progress-dark-dot
                         'year-progress-light-dot)))
            (insert (propertize dot 'face face))
            (insert " ")
            (when (and (= (mod day 30) 29) (< day (- days-in-year 1)))
              (insert "\n"))))
        (insert "\n\n")
        (insert (format "Days passed: %d\n" current-day-of-year))
        (insert (format "Days remaining: %d\n" (- days-in-year current-day-of-year)))
        (insert (format "Progress: %.1f%%\n"
                        (* 100.0 (/ current-day-of-year (float days-in-year)))))
        (insert (format "Leap year: %s\n" (if is-leap-year "Yes" "No"))))
      (unless (derived-mode-p 'progress-mode)
        (progress-mode))
      (goto-char (point-min))
      (display-buffer buffer))))

(provide 'progress-mode)
;;; progress-mode.el ends here
